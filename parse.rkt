#lang racket/base

(require parser-tools/lex
         racket/contract
         racket/generator
         racket/list
         syntax/parse
         syntax/stx
         (prefix-in es: "ast.rkt")
         "private/grammar.rkt"
         "private/lex.rkt")

(provide/contract
 [read-program
  (->* (input-port?)
       (or/c eof-object? (listof es:source-element?)))])

(define (read-program [in (current-output-port)])
  (define prog
    (parse-program
     (collapse-syntax
      (strip-whitespace
       (parse
        (insert-block-semicolons
         (λ ()
           (lex in))))))))
  (if (null? prog)
      eof
      prog))

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
  (syntax-parse stx
    [(rule sub ...)
     #`(rule
        #,@(filter-map
            (λ (stx)
              (syntax-parse stx
                [((~datum ws) . _) #f]
                [((~datum ws-no-eol) . _) #f]
                [_ (strip-whitespace stx)]))
            (syntax->list #'(sub ...))))]
    [_ stx]))

(define (collapse-syntax stx)
  (syntax-parse stx
    [((~datum eol-or-semicolon) _) #'";"]
    [(rule sub ...)
     (let ([subs (syntax->list #'(sub ...))])
       (if (and (= 1 (length subs))
                (not
                 (memq (syntax-e #'rule)
                       '(program identifier numeric string
                         statement variable-declaration-list
                         variable-declaration-list-no-in
                         variable-declaration
                         variable-declaration-no-in
                         empty-statement
                         formal-parameter-list))))
           (collapse-syntax (car subs))
           #`(rule #,@(map collapse-syntax subs))))]
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
     (es:decl:fn (stx-loc stx) (parse-function stx))]))

(define (parse-expression stx)
  (define loc (stx-loc stx))
  (syntax-parse stx
    #:datum-literals (primary-expression identifier numeric string
                      comma-expression member-expression new-expression
                      call-expression postfix-expression unary-expression
                      conditional-expression assignment-expression
                      assignment-operator expression)
    [(primary-expression "this") (es:expr:this loc)]
    [(identifier id) (es:expr:id loc (syntax-e #'id))]
    [(primary-expression "null") (es:expr:null loc)]
    [(primary-expression "true") (es:expr:bool loc #t)]
    [(primary-expression "false") (es:expr:bool loc #f)]
    [(numeric v) (es:expr:number loc (datum-intern-literal (syntax-e #'v)))]
    [(string v) (es:expr:string loc (datum-intern-literal (syntax-e #'v)))]
    [(array-literal "[" (~seq expr ...) "]") (error "TODO: array literal")]
    [(object-literal "{" (~seq prop ...) "}") (error "TODO: object literal")]
    [(primary-expression "(" expr ")") (parse-expression #'expr)]
    [(_ left "," right)
     (es:expr:comma loc (parse-expression #'left)
                    (parse-expression #'right))]
    [(_ obj "[" prop "]")
     (es:expr:member loc (parse-expression #'obj)
                     (parse-expression #'prop))]
    [(_ obj "." (identifier prop))
     (es:expr:member loc (parse-expression #'obj)
                     (syntax-e #'prop))]
    [(_ "new" expr (~optional args))
     (es:expr:new loc (parse-expression #'expr)
                  (if (attribute args)
                      (parse-arguments #'args)
                      '()))]
    [(call-expression expr args)
     (es:expr:call loc (parse-expression #'expr)
                   (parse-arguments #'args))]
    [(postfix-expression expr op)
     (es:expr:postfix loc (parse-expression #'expr)
                      (string->symbol (syntax-e #'op)))]
    [(unary-expression op expr)
     (es:expr:unary loc (string->symbol (syntax-e #'op))
                    (parse-expression #'expr))]
    [(_ left (assignment-operator op) right)
     (es:expr:binary loc (parse-expression #'left)
                     (string->symbol (syntax-e #'op))
                     (parse-expression #'right))]
    [(_ left op:str right)
     (es:expr:binary loc (parse-expression #'left)
                     (string->symbol (syntax-e #'op))
                     (parse-expression #'right))]
    [(_ test "?" true ":" false)
     (es:expr:cond loc (parse-expression #'test)
                   (parse-expression #'true)
                   (parse-expression #'false))]))

(define (parse-arguments stx)
  (syntax-parse stx
    #:datum-literals (arguments)
    [(arguments "(" (~seq (~optional ",") expr) ... ")")
     (stx-map parse-expression #'(expr ...))]))

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
     (es:stmt:block loc (stx-map parse-statement #'(stmt ...)))]
    [(variable-statement "var" declist ";")
     (es:stmt:vars loc (parse-variable-declaration-list #'declist))]
    [(empty-statement ";")
     (es:stmt:empty loc)]
    [(expression-statement expr ";")
     (es:stmt:expr loc (parse-expression #'expr))]
    [(if-statement "if" "(" test ")" true (~optional (~seq "else" false)))
     (es:stmt:if loc (parse-expression #'test)
                 (parse-statement #'true)
                 (and (attribute false) (parse-statement #'false)))]
    [(iteration-statement "do" stmt "while" "(" test ")" ";")
     (es:stmt:do loc (parse-expression #'stmt)
                 (parse-expression #'test))]
    [(iteration-statement "while" "(" test ")" body)
     (es:stmt:while loc (parse-expression #'test)
                    (parse-statement #'body))]
    [(iteration-statement "for" "(" "var" declist ";" (~optional test) ";" (~optional step) ")" body)
     (es:stmt:for loc (parse-variable-declaration-list #'declist)
                  (and (attribute test) (parse-expression #'test))
                  (and (attribute step) (parse-expression #'step))
                  (parse-statement #'body))]
    [(iteration-statement "for" "(" (~optional init) ";" (~optional test) ";" (~optional step) ")" body)
     (es:stmt:for loc (and (attribute init) (parse-expression #'init))
                  (and (attribute test) (parse-expression #'test))
                  (and (attribute step) (parse-expression #'step))
                  (parse-statement #'body))]
    [(iteration-statement "for" "(" "var" decl "in" expr ")" body)
     (es:stmt:for-in loc (parse-variable-declaration #'decl)
                     (parse-expression #'expr)
                     (parse-statement #'body))]
    [(iteration-statement "for" "(" init "in" expr ")" body)
     (es:stmt:for-in loc (parse-expression #'init)
                     (parse-expression #'expr)
                     (parse-statement #'body))]
    [(continue-statement "continue" (~optional (identifier id)) ";")
     (es:stmt:continue loc (and (attribute id) (syntax-e #'id)))]
    [(break-statement "break" (~optional (identifier id)) ";")
     (es:stmt:break loc (and (attribute id) (syntax-e #'id)))]
    [(return-statement "return" (~optional expr) ";")
     (es:stmt:return loc (and (attribute expr) (parse-expression #'expr)))]
    [(with-statement "with" "(" expr ")" body)
     (es:stmt:with loc (parse-expression #'expr)
                   (parse-statement #'body))]
    [(switch-statement "(" expr ")" ((~datum case-block) "{" clause ... "}"))
     (error "TODO: switch")]
    [(labelled-statement (identifier label) ":" stmt)
     (es:stmt:labelled (syntax-e #'label) (parse-statement #'stmt))]
    [(try-statement "try" block
                    (~optional ((~datum catch) "catch" "(" (identifier catch-id) ")" catch-block))
                    (~optional ((~datum finally) "finally" finally-block)))
     (error "TODO: try")]
    [(debugger-statement "debugger" ";")
     (es:stmt:debugger loc)]))

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
     (es:decl:var (stx-loc stx) (syntax-e #'id)
                  (and (attribute expr) (parse-expression #'expr)))]))

(define (parse-function stx)
  (syntax-parse stx
    #:datum-literals (function-declaration formal-parameter-list identifier)
    [((~datum function-declaration)
      "function" (identifier id) "("
      (~optional (formal-parameter-list
                  (identifier param0)
                  (~seq "," (identifier param)) ...))
      ")" "{" body ... "}")
     (es:fn (stx-loc stx) (and (attribute id) (syntax-e #'id))
            (if (attribute param0)
                (stx-map syntax-e #'(param0 param ...))
                '())
            (stx-map parse-source-element #'(body ...)))]))
