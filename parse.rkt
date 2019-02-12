#lang racket/base

(require parser-tools/lex
         racket/contract
         racket/generator
         racket/list
         syntax/parse
         syntax/srcloc
         syntax/stx
         (prefix-in ast: "ast.rkt")
         "lang/grammar.rkt"
         "lang/lex.rkt")

(provide/contract
 [read-program
  (->* ()
       (any/c input-port?)
       (or/c (listof (or/c ast:function? ast:statement?))
             eof-object?))])

(define (read-program [source-name (object-name (current-input-port))]
                      [in (current-input-port)])
  (define raw
    (parse
     (insert-block-semicolons
      (λ ()
        (lex in)))))
  (define prog
    (parse-program
     (collapse-syntax
      (strip-whitespace
       (add-source-name source-name raw)))))
  (if (null? prog)
      eof
      prog))

(define (add-source-name src stx)
  (datum->syntax #f
    (let ([subs (syntax->list stx)])
      (if subs
          (map
           (λ (s)
             (add-source-name src s))
           subs)
          stx))
    (update-source-location (stx-loc stx)
                            #:source src)))

(define (insert-block-semicolons next-token)
  (define (needs-insert? token)
    (not
     (member token '(EOL ";" "{" "}"))))
  (generator ()
   (let loop ([last-non-ws 'EOL])
     (define pt (next-token))
     (if (eq? 'EOF pt)
         (begin
           (when (needs-insert? last-non-ws)
             (yield 'INSERTED-SEMICOLON))
           pt)
         (let ([t (position-token-token pt)])
           (when (and (equal? "}" t)
                      (needs-insert? last-non-ws))
             (yield 'INSERTED-SEMICOLON))
           (yield pt)
           (loop (if (memq t '(COMMENT WS))
                     last-non-ws
                     t)))))))

(define (strip-whitespace stx)
  (let ([subs (syntax->list stx)])
    (if subs
        (datum->syntax #f
          (cons
           (car subs)
           (filter-map
            (λ (stx)
              (syntax-parse stx
                [((~datum ws) . _) #f]
                [((~datum ws-no-eol) . _) #f]
                [_ (strip-whitespace stx)]))
            (cdr subs)))
          (stx-loc stx))
        stx)))

(define (collapse-syntax stx)
  (syntax-parse stx
    [((~datum eol-or-semicolon) _) #'";"]
    [(_ . _)
     (let ([subs (syntax->list stx)])
       (if (and (= 2 (length subs))
                (identifier? (car subs))
                (not
                 (memq (syntax-e (car subs))
                       '(program primary-expression
                         identifier numeric string regexp
                         statement variable-declaration-list
                         variable-declaration-list-no-in
                         variable-declaration
                         variable-declaration-no-in
                         empty-statement
                         formal-parameter-list))))
           (collapse-syntax (cadr subs))
           (datum->syntax #f
             (cons
              (car subs)
              (map collapse-syntax (cdr subs)))
             (stx-loc stx))))]
    [_ stx]))

(define (stx-loc stx)
  (list
   (syntax-source stx)
   (syntax-line stx)
   (syntax-column stx)
   (syntax-position stx)
   (syntax-span stx)))

(define (parse-program stx)
  (syntax-parse stx
    [((~datum program) elt ...)
     (stx-map parse-source-element #'(elt ...))]))

(define (parse-source-element stx)
  (syntax-parse stx
    [((~datum statement) . _)
     (parse-statement stx)]
    [((~datum function-declaration) . _)
     (parse-function stx)]))

(define (parse-expression stx)
  (define loc (stx-loc stx))
  (syntax-parse stx
    #:datum-literals (primary-expression identifier numeric string regexp
                      comma-expression member-expression new-expression
                      call-expression postfix-expression unary-expression
                      conditional-expression assignment-expression
                      function-expression assignment-operator expression)
    [(primary-expression "this")
     (ast:expression:this loc)]
    [(primary-expression (identifier id))
     (ast:expression:reference loc (ast:identifier loc (syntax-e #'id)))]
    [(primary-expression "null")
     (ast:expression:literal loc (ast:literal:null loc))]
    [(primary-expression "true")
     (ast:expression:literal loc (ast:literal:boolean loc #t))]
    [(primary-expression "false")
     (ast:expression:literal loc (ast:literal:boolean loc #f))]
    [(primary-expression (numeric v))
     (ast:expression:literal loc (ast:literal:number loc (datum-intern-literal (syntax-e #'v))))]
    [(primary-expression (string v))
     (ast:expression:literal loc (ast:literal:string loc (datum-intern-literal (syntax-e #'v))))]
    [(primary-expression (regexp (pattern flags)))
     (ast:expression:literal
      loc
      (ast:literal:regexp
       loc
       (datum-intern-literal (syntax-e #'pattern))
       (syntax->datum #'flags)))]
    [(primary-expression (~and ((~datum array-literal) . _) arry))
     (ast:expression:literal
      loc
      (parse-array-literal #'arry))]
    [(primary-expression (~and ((~datum object-literal) . _) obj))
     (ast:expression:literal
      loc
      (parse-object-literal #'obj))]
    [(primary-expression "(" expr ")")
     (parse-expression #'expr)]
    [(function-expression . _)
     (ast:expression:function
      loc
      (parse-function stx))]
    [(_ left "," right)
     (ast:expression:comma
      loc
      (parse-expression #'left)
      (parse-expression #'right))]
    [(_ obj "[" prop "]")
     (ast:expression:member-reference
      loc
      (parse-expression #'obj)
      (parse-expression #'prop))]
    [(_ obj "." (identifier prop))
     (ast:expression:member-reference
      loc
      (parse-expression #'obj)
      (ast:identifier (stx-loc #'prop) (syntax-e #'prop)))]
    [(_ "new" expr (~optional args))
     (ast:expression:new
      loc
      (parse-expression #'expr)
      (if (attribute args)
          (parse-arguments #'args)
          '()))]
    [(call-expression expr args)
     (ast:expression:call
      loc
      (parse-expression #'expr)
      (parse-arguments #'args))]
    [(postfix-expression expr op)
     (ast:expression:postfix
      loc
      (parse-expression #'expr)
      (ast:operator (stx-loc #'op) (string->symbol (syntax-e #'op))))]
    [(unary-expression op expr)
     (ast:expression:unary
      loc
      (ast:operator (stx-loc #'op) (string->symbol (syntax-e #'op)))
      (parse-expression #'expr))]
    [(_ left (assignment-operator op) right)
     (ast:expression:binary
      loc
      (parse-expression #'left)
      (ast:operator (stx-loc #'op) (string->symbol (syntax-e #'op)))
      (parse-expression #'right))]
    [(_ left op:str right)
     (ast:expression:binary
      loc
      (parse-expression #'left)
      (ast:operator (stx-loc #'op) (string->symbol (syntax-e #'op)))
      (parse-expression #'right))]
    [(_ test "?" true ":" false)
     (ast:expression:conditional
      loc
      (parse-expression #'test)
      (parse-expression #'true)
      (parse-expression #'false))]))

(define (parse-arguments stx)
  (syntax-parse stx
    #:datum-literals (arguments)
    [(arguments "(" (~seq (~optional ",") expr) ... ")")
     (stx-map parse-expression #'(expr ...))]))

(define (parse-array-literal stx)
  (syntax-parse stx
    [((~datum array-literal) "[" term ... "]")
     (ast:literal:array
      (stx-loc stx)
      (let loop ([terms (syntax->list #'(term ...))])
        (cond
          [(null? terms) '()]
          [(equal? "," (syntax-e (car terms)))
           (cons #f (loop (cdr terms)))]
          [(cons (parse-expression (car terms))
                 (if (null? (cdr terms))
                     '()
                     (loop (cddr terms))))])))]))

(define (parse-object-literal stx)
  (define-syntax-class property-name
    #:attributes (ast)
    #:datum-literals (identifier string numeric)
    [pattern (identifier id)
             #:attr ast (ast:identifier (stx-loc #'id) (syntax-e #'id))]
    [pattern (string s)
             #:attr ast (ast:literal:string (stx-loc #'s) (syntax-e #'s))]
    [pattern (numeric n)
             #:attr ast (ast:literal:number (stx-loc #'n) (syntax-e #'n))])
  (syntax-parse stx
    #:datum-literals (object-literal property-assignment)
    [(object-literal "{" (~seq (~and (property-assignment . _) prop)
                               (~optional ",")) ...
                     (~optional (~datum INSERTED-SEMICOLON)) "}")
     (ast:literal:object
      (stx-loc stx)
      (stx-map
       (λ (pstx)
         (syntax-parse pstx
           #:datum-literals (get set)
           [(_ name:property-name ":" expr)
            (ast:property-initializer:data
             (stx-loc pstx)
             (attribute name.ast)
             (parse-expression #'expr))]
           [(_ get name:property-name "(" ")" "{" body ... "}")
            (ast:property-initializer:get
             (stx-loc pstx)
             (attribute name.ast)
             (ast:function
              (stx-loc pstx)
              #f
              '()
              (stx-map parse-source-element #'(body ...))))]
           [(_ set name:property-name "(" ((~datum identifier) id) ")" "{" body ... "}")
            (ast:property-initializer:set
             (stx-loc pstx)
             (attribute name.ast)
             (ast:function
              (stx-loc pstx)
              #f
              (list (ast:identifier (stx-loc #'id) (syntax-e #'id)))
              (stx-map parse-source-element #'(body ...))))]))
       #'(prop ...)))]))

(define (parse-statement stx)
  (define loc (stx-loc stx))
  (syntax-parse stx
    #:datum-literals (statement block variable-statement empty-statement
                      expression-statement if-statement iteration-statement
                      continue-statement break-statement return-statement
                      with-statement switch-statement labelled-statement
                      throw-statement try-statement debugger-statement
                      identifier)
    [(statement s)
     (parse-statement #'s)]
    [(block "{" stmt ... "}")
     (ast:statement:block loc (stx-map parse-statement #'(stmt ...)))]
    [(variable-statement "var" declist ";")
     (ast:statement:var loc (parse-variable-declaration-list #'declist))]
    [(empty-statement ";")
     (ast:statement:empty loc)]
    [(expression-statement expr ";")
     (ast:statement:expression loc (parse-expression #'expr))]
    [(if-statement "if" "(" test ")" true (~optional (~seq "else" false)))
     (ast:statement:if
      loc
      (parse-expression #'test)
      (parse-statement #'true)
      (and (attribute false) (parse-statement #'false)))]
    [(iteration-statement "do" stmt "while" "(" test ")" ";")
     (ast:statement:do
      loc
      (parse-expression #'stmt)
      (parse-expression #'test))]
    [(iteration-statement "while" "(" test ")" body)
     (ast:statement:while
      loc
      (parse-expression #'test)
      (parse-statement #'body))]
    [(iteration-statement "for" "(" "var" declist ";" (~optional test) ";" (~optional step) ")" body)
     (ast:statement:for
      loc
      (parse-variable-declaration-list #'declist)
      (and (attribute test) (parse-expression #'test))
      (and (attribute step) (parse-expression #'step))
      (parse-statement #'body))]
    [(iteration-statement "for" "(" (~optional init) ";" (~optional test) ";" (~optional step) ")" body)
     (ast:statement:for
      loc
      (and (attribute init) (parse-expression #'init))
      (and (attribute test) (parse-expression #'test))
      (and (attribute step) (parse-expression #'step))
      (parse-statement #'body))]
    [(iteration-statement "for" "(" "var" decl "in" expr ")" body)
     (ast:statement:for-in
      loc
      (parse-variable-declaration #'decl)
      (parse-expression #'expr)
      (parse-statement #'body))]
    [(iteration-statement "for" "(" init "in" expr ")" body)
     (ast:statement:for-in
      loc
      (parse-expression #'init)
      (parse-expression #'expr)
      (parse-statement #'body))]
    [(continue-statement "continue" (~optional (identifier id)) ";")
     (ast:statement:continue
      loc
      (and (attribute id) (ast:identifier (stx-loc #'id) (syntax-e #'id))))]
    [(break-statement "break" (~optional (identifier id)) ";")
     (ast:statement:break
      loc
      (and (attribute id) (ast:identifier (stx-loc #'id) (syntax-e #'id))))]
    [(return-statement "return" (~optional expr) ";")
     (ast:statement:return
      loc
      (and (attribute expr) (parse-expression #'expr)))]
    [(with-statement "with" "(" expr ")" body)
     (ast:statement:with
      loc
      (parse-expression #'expr)
      (parse-statement #'body))]
    [(switch-statement "switch" "(" expr ")" ((~datum case-block) "{" clause ... "}"))
     (ast:statement:switch
      loc
      (parse-expression #'expr)
      (map
       (λ (stx)
         (syntax-parse stx
           [((~datum case-clause) "case" expr ":" stmt ...)
            (ast:case-clause
             (stx-loc stx)
             (parse-expression #'expr)
             (map parse-statement (attribute stmt)))]
           [((~datum default-clause) "default" ":" stmt ...)
            (ast:default-clause
             (stx-loc stx)
             (map parse-statement (attribute stmt)))]))
       (attribute clause)))]
    [(labelled-statement (identifier label) ":" stmt)
     (ast:statement:label
      loc
      (ast:identifier (stx-loc #'label) (syntax-e #'label))
      (parse-statement #'stmt))]
    [(throw-statement "throw" expr ";")
     (ast:statement:throw loc (parse-expression #'expr))]
    [(try-statement "try" try-block
                    (~optional ((~datum catch) "catch" "(" (identifier catch-id) ")" catch-block))
                    (~optional ((~datum finally) "finally" finally-block)))
     (ast:statement:try
      loc
      (parse-statement #'try-block)
      (and (attribute catch-id) (ast:identifier (stx-loc #'catch-id) (syntax-e #'catch-id)))
      (and (attribute catch-block) (parse-statement #'catch-block))
      (and (attribute finally-block) (parse-statement #'finally-block)))]
    [(debugger-statement "debugger" ";")
     (ast:statement:debugger loc)]))

(define (parse-variable-declaration-list stx)
  (syntax-parse stx
    [((~or (~datum variable-declaration-list)
           (~datum variable-declaration-list-no-in))
      decl0 (~seq "," decl) ...)
     (stx-map parse-variable-declaration #'(decl0 decl ...))]))

(define (parse-variable-declaration stx)
  (syntax-parse stx
    [((~or (~datum variable-declaration)
           (~datum variable-declaration-no-in))
      ((~datum identifier) id) (~optional (~seq "=" expr)))
     (ast:variable-declaration
      (stx-loc stx)
      (ast:identifier (stx-loc #'id) (syntax-e #'id))
      (and (attribute expr) (parse-expression #'expr)))]))

(define (parse-function stx)
  (syntax-parse stx
    #:datum-literals (function-declaration function-expression
                      formal-parameter-list identifier)
    [((~or function-declaration function-expression)
      "function" (~optional (identifier id)) "("
      (~optional (formal-parameter-list
                  (identifier param0)
                  (~seq "," (identifier param)) ...))
      ")" "{" body ... "}")
     (ast:function
      (stx-loc stx)
      (and (attribute id) (ast:identifier (stx-loc #'id) (syntax-e #'id)))
      (if (attribute param0)
          (stx-map
           (λ (pstx)
             (ast:identifier (stx-loc pstx) (syntax-e pstx)))
           #'(param0 param ...))
          '())
      (stx-map parse-source-element #'(body ...)))]))
