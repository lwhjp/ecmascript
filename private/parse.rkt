#lang racket/base

(module+ test
  (require racket/sequence)
  (call-with-input-file "/home/leo/code/test.js" es-parse))

(require parser-tools/lex
         racket/generator
         racket/list
         syntax/parse
         syntax/stx
         "grammar.rkt"
         "lex.rkt"
         "syntax.rkt")

(provide es-parse)

(define (in-tokens in)
  (in-producer es-lex 'EOF in))

(define (insert-block-semicolons seq)
  (define-values (has-more? next)
    (sequence-generate seq))
  (define (needs-insert? token)
    (not
     (member token '(EOL ";" "{" "}"))))
  (in-generator
   (let loop ([last-non-ws #f])
     (cond
       [(has-more?)
        (let* ([pt (next)]
               [t (position-token-token pt)])
          (when (and (equal? "}" t)
                     (needs-insert? last-non-ws))
            (yield '";"))
          (yield pt)
          (loop (if (memq t '(COMMENT WS))
                    last-non-ws
                    t)))]
       [else
        (when (needs-insert? last-non-ws)
          (yield ";"))]))))

(define (port->raw-syntax in)
  (parse
   (insert-block-semicolons
    (in-tokens in))))

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

(define (es-parse in)
  (define stx
    (collapse-syntax
     (strip-whitespace
      (port->raw-syntax in))))
  (parse-program stx))

(define (parse-program stx)
  (syntax-parse stx
    [((~datum program) elt ...)
     (es-program stx (stx-map parse-source-element #'(elt ...)))]))

(define (parse-source-element stx)
  (syntax-parse stx
    [((~datum statement) . _) (parse-statement stx)]
    [((~datum function-declaration) . _) (parse-function stx)]))

(define (parse-expression stx)
  (syntax-parse stx
    #:datum-literals (primary-expression identifier numeric string
                      comma-expression member-expression new-expression
                      call-expression postfix-expression unary-expression
                      conditional-expression assignment-expression
                      assignment-operator expression)
    [(primary-expression "this") (es-this stx)]
    [(identifier id) (es-id #'id (syntax-e #'id))]
    [(numeric v) (es-numeric stx (syntax-e #'v))]
    [(string v) (es-string stx (syntax-e #'v))]
    [(primary-expression "null") (es-null stx)]
    [(primary-expression "true") (es-bool stx #t)]
    [(primary-expression "false") (es-bool stx #f)]
    [(primary-expression "(" expr ")") (parse-expression #'expr)]
    [(comma-expression left "," right) (es-comma-expr stx (parse-expression #'left) (parse-expression #'right))]
    [(_ obj "[" prop "]") (es-member-expr stx (parse-expression #'obj) (parse-expression #'prop))]
    [(_ obj "." prop) (es-member-expr stx (parse-expression #'obj) (parse-id #'prop))]
    [(_ "new" expr args) (es-new-expr stx (parse-expression #'expr) (parse-arguments #'args))]
    [(_ "new" expr) (es-new-expr stx (parse-expression #'expr) '())]
    [(call-expression expr args) (es-call-expr stx (parse-expression #'expr) (parse-arguments #'args))]
    [(postfix-expression expr op) (es-postfix-expr stx (parse-expression #'expr) (string->symbol (syntax-e #'op)))]
    [(unary-expression op expr) (es-unary-expr stx (string->symbol (syntax-e #'op)) (parse-expression #'expr))]
    [(assignment-expression left (assignment-operator op) right)
     (es-binary-expr stx (parse-expression #'left) (string->symbol (syntax-e #'op)) (parse-expression #'right))]
    [(_ left op:str right)
     (es-binary-expr stx (parse-expression #'left) (string->symbol (syntax-e #'op)) (parse-expression #'right))]
    [(conditional-expression test "?" true ":" false)
     (es-ternary-expr stx (parse-expression #'test) (parse-expression #'true) (parse-expression #'false))]))

(define (parse-id stx)
  (syntax-parse stx
    #:datum-literals (identifier)
    [(identifier id) (es-id stx (syntax-e #'id))]))

(define (parse-arguments stx)
  (syntax-parse stx
    #:datum-literals (arguments)
    [(arguments "(" (~seq (~optional ",") expr) ... ")")
     (stx-map parse-expression #'(expr ...))]))

(define (parse-statement stx)
  (syntax-parse stx
    #:datum-literals (statement block variable-statement empty-statement expression-statement
                      if-statement iteration-statement continue-statement
                      break-statement return-statement with-statement)
    [(statement s) (parse-statement #'s)]
    [(block "{" stmt ... "}") (es-block stx (stx-map parse-statement #'(stmt ...)))]
    [(variable-statement "var" declist ";") (es-var-stmt stx (parse-variable-declaration-list #'declist))]
    [(empty-statement ";") (es-empty-stmt stx)]
    [(expression-statement expr ";") (es-expr-stmt stx (parse-expression #'expr))]
    [(if-statement "if" "(" test ")" true) (es-if stx (parse-expression #'test) (parse-statement #'true) #f)]
    [(if-statement "if" "(" test ")" true "else" false)
     (es-if stx (parse-expression #'test) (parse-statement #'true) (parse-statement #'false))]
    [(iteration-statement "while" "(" test ")" body) (es-while stx (parse-expression #'test) (parse-statement #'body))]
    [(iteration-statement "for" "(" "var" declist ";" (~optional test) ";" (~optional step) ")" body)
     (es-for stx (parse-variable-declaration-list #'declist) (parse-optexpr (attribute test))
             (parse-optexpr (attribute step)) (parse-statement #'body))]
    [(iteration-statement "for" "(" (~optional init) ";" (~optional test) ";" (~optional step) ")" body)
     (es-for stx (parse-optexpr (attribute init)) (parse-optexpr (attribute test))
             (parse-optexpr (attribute step)) (parse-statement #'body))]
    [(iteration-statement "for" "(" "var" (~seq id (~optional (~seq "=" init))) "in" expr ")" body)
     (es-for-in stx (es-var-decl #'id (parse-id #'id) (parse-optexpr (attribute init)))
                (parse-expression #'expr) (parse-statement #'body))]
    [(iteration-statement "for" "(" init "in" expr ")" body)
     (es-for-in stx (parse-expression #'init) (parse-expression #'expr) (parse-statement #'body))]
    [(continue-statement "continue" ";") (es-continue stx)]
    [(break-statement "break" ";") (es-break stx)]
    [(return-statement "return" (~optional expr) ";") (es-return stx (parse-optexpr (attribute expr)))]
    [(with-statement "with" "(" expr ")" body) (es-with stx (parse-expression #'expr) (parse-statement #'body))]))

(define (parse-optexpr v)
  (and v (parse-expression v)))

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
      id (~optional (~seq "=" expr)))
     (es-var-decl stx (parse-id #'id) (parse-optexpr (attribute expr)))]))

(define (parse-function stx)
  (syntax-parse stx
    [((~datum function-declaration)
      "function" id "("
      (~optional ((~datum formal-parameter-list)
                  arg0 (~seq "," arg) ...))
      ")" "{" body ... "}")
     (es-function
      stx
      (parse-id #'id)
      (if (attribute arg0) (stx-map parse-id #'(arg0 arg ...)) '())
      (parse-statement #'(block "{" body ... "}")))]))
