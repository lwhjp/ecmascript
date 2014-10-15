#lang racket/base

(require parser-tools/lex
         racket/contract
         racket/generator
         racket/list
         syntax/parse
         syntax/srcloc
         syntax/stx
         (prefix-in ecma: "ast.rkt")
         "private/grammar.rkt"
         "private/lex.rkt")

(provide/contract
 [read-program
  (->* ()
       (any/c input-port?)
       (or/c eof-object? (listof ecma:source-element?)))])

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
             (yield ";"))
           pt)
         (let ([t (position-token-token pt)])
           (when (and (equal? "}" t)
                      (needs-insert? last-non-ws))
             (yield ";"))
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
                (not
                 (memq (syntax-e (car subs))
                       '(program primary-expression
                         identifier numeric string
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
     (ecma:decl:fn (stx-loc stx) (parse-function stx))]))

(define (parse-expression stx)
  (define loc (stx-loc stx))
  (syntax-parse stx
    #:datum-literals (primary-expression identifier numeric string
                      comma-expression member-expression new-expression
                      call-expression postfix-expression unary-expression
                      conditional-expression assignment-expression
                      function-expression assignment-operator expression)
    [(primary-expression "this") (ecma:expr:this loc)]
    [(primary-expression (identifier id)) (ecma:expr:id loc (syntax-e #'id))]
    [(primary-expression "null") (ecma:expr:null loc)]
    [(primary-expression "true") (ecma:expr:bool loc #t)]
    [(primary-expression "false") (ecma:expr:bool loc #f)]
    [(primary-expression (numeric v))
     (ecma:expr:number loc (datum-intern-literal (syntax-e #'v)))]
    [(primary-expression (string v))
     (ecma:expr:string loc (datum-intern-literal (syntax-e #'v)))]
    [(primary-expression (array-literal "[" (~seq expr ...) "]"))
     (error "TODO: array literal")]
    [(primary-expression (~and ((~datum object-literal) . _) obj))
     (parse-object-literal #'obj)]
    [(primary-expression "(" expr ")") (parse-expression #'expr)]
    [(function-expression . _) (ecma:expr:fn loc (parse-function stx))]
    [(_ left "," right)
     (ecma:expr:comma loc (parse-expression #'left) (parse-expression #'right))]
    [(_ obj "[" prop "]")
     (ecma:expr:member loc (parse-expression #'obj) (parse-expression #'prop))]
    [(_ obj "." (identifier prop))
     (ecma:expr:member loc (parse-expression #'obj) (syntax-e #'prop))]
    [(_ "new" expr (~optional args))
     (ecma:expr:new loc (parse-expression #'expr)
                    (if (attribute args)
                        (parse-arguments #'args)
                        '()))]
    [(call-expression expr args)
     (ecma:expr:call loc (parse-expression #'expr) (parse-arguments #'args))]
    [(postfix-expression expr op)
     (ecma:expr:postfix loc (parse-expression #'expr) (string->symbol (syntax-e #'op)))]
    [(unary-expression op expr)
     (ecma:expr:unary loc (string->symbol (syntax-e #'op))
                      (parse-expression #'expr))]
    [(_ left (assignment-operator op) right)
     (ecma:expr:binary loc (parse-expression #'left)
                       (string->symbol (syntax-e #'op))
                       (parse-expression #'right))]
    [(_ left op:str right)
     (ecma:expr:binary loc (parse-expression #'left)
                       (string->symbol (syntax-e #'op))
                       (parse-expression #'right))]
    [(_ test "?" true ":" false)
     (ecma:expr:cond loc (parse-expression #'test)
                     (parse-expression #'true)
                     (parse-expression #'false))]))

(define (parse-arguments stx)
  (syntax-parse stx
    #:datum-literals (arguments)
    [(arguments "(" (~seq (~optional ",") expr) ... ")")
     (stx-map parse-expression #'(expr ...))]))

(define (parse-object-literal stx)
  (define parse-name
    (syntax-parser
      [((~datum identifier) id) (syntax-e #'id)]
      [((~datum string) s) (syntax-e #'s)]
      [((~datum numeric) n) (syntax-e #'n)]))
  (syntax-parse stx
    #:datum-literals (object-literal property-assignment)
    [(object-literal "{" (~seq (~and (property-assignment . _) prop) (~optional ",")) ... "}")
     (ecma:expr:object
      (stx-loc stx)
      (stx-map
       (λ (pstx)
         (syntax-parse pstx
           #:datum-literals (get set)
           [(_ name ":" expr)
            (ecma:init:obj:prop
             (stx-loc pstx)
             (parse-name #'name)
             (parse-expression #'expr))]
           [(_ get name "(" ")" "{" body ... "}")
            (ecma:init:obj:get
             (stx-loc pstx)
             (parse-name #'name)
             (ecma:fn (stx-loc pstx)
                      #f
                      '()
                      (stx-map parse-source-element #'(body ...))))]
           [(_ set name "(" ((~datum identifier) id) ")" "{" body ... "}")
            (ecma:init:obj:set
             (stx-loc pstx)
             (parse-name #'name)
             (ecma:fn (stx-loc pstx)
                      #f
                      (list (syntax-e #'id))
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
     (ecma:stmt:block loc (stx-map parse-statement #'(stmt ...)))]
    [(variable-statement "var" declist ";")
     (ecma:stmt:vars loc (parse-variable-declaration-list #'declist))]
    [(empty-statement ";")
     (ecma:stmt:empty loc)]
    [(expression-statement expr ";")
     (ecma:stmt:expr loc (parse-expression #'expr))]
    [(if-statement "if" "(" test ")" true (~optional (~seq "else" false)))
     (ecma:stmt:if loc (parse-expression #'test)
                   (parse-statement #'true)
                   (and (attribute false) (parse-statement #'false)))]
    [(iteration-statement "do" stmt "while" "(" test ")" ";")
     (ecma:stmt:do loc (parse-expression #'stmt)
                   (parse-expression #'test))]
    [(iteration-statement "while" "(" test ")" body)
     (ecma:stmt:while loc (parse-expression #'test)
                      (parse-statement #'body))]
    [(iteration-statement "for" "(" "var" declist ";" (~optional test) ";" (~optional step) ")" body)
     (ecma:stmt:for loc (parse-variable-declaration-list #'declist)
                    (and (attribute test) (parse-expression #'test))
                    (and (attribute step) (parse-expression #'step))
                    (parse-statement #'body))]
    [(iteration-statement "for" "(" (~optional init) ";" (~optional test) ";" (~optional step) ")" body)
     (ecma:stmt:for loc (and (attribute init) (parse-expression #'init))
                    (and (attribute test) (parse-expression #'test))
                    (and (attribute step) (parse-expression #'step))
                    (parse-statement #'body))]
    [(iteration-statement "for" "(" "var" decl "in" expr ")" body)
     (ecma:stmt:for-in loc (parse-variable-declaration #'decl)
                       (parse-expression #'expr)
                       (parse-statement #'body))]
    [(iteration-statement "for" "(" init "in" expr ")" body)
     (ecma:stmt:for-in loc (parse-expression #'init)
                       (parse-expression #'expr)
                       (parse-statement #'body))]
    [(continue-statement "continue" (~optional (identifier id)) ";")
     (ecma:stmt:continue loc (and (attribute id) (syntax-e #'id)))]
    [(break-statement "break" (~optional (identifier id)) ";")
     (ecma:stmt:break loc (and (attribute id) (syntax-e #'id)))]
    [(return-statement "return" (~optional expr) ";")
     (ecma:stmt:return loc (and (attribute expr) (parse-expression #'expr)))]
    [(with-statement "with" "(" expr ")" body)
     (ecma:stmt:with loc (parse-expression #'expr)
                     (parse-statement #'body))]
    [(switch-statement "(" expr ")" ((~datum case-block) "{" clause ... "}"))
     (error "TODO: switch")]
    [(labelled-statement (identifier label) ":" stmt)
     (ecma:stmt:labelled loc (syntax-e #'label) (parse-statement #'stmt))]
    [(throw-statement "throw" expr ";")
     (ecma:stmt:throw loc (parse-expression #'expr))]
    [(try-statement "try" block
                    (~optional ((~datum catch) "catch" "(" (identifier catch-id) ")" catch-block))
                    (~optional ((~datum finally) "finally" finally-block)))
     (error "TODO: try")]
    [(debugger-statement "debugger" ";")
     (ecma:stmt:debugger loc)]))

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
     (ecma:decl:var (stx-loc stx) (syntax-e #'id)
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
     (ecma:fn (stx-loc stx) (and (attribute id) (syntax-e #'id))
              (if (attribute param0)
                  (stx-map syntax-e #'(param0 param ...))
                  '())
              (stx-map parse-source-element #'(body ...)))]))
