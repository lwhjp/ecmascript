#lang racket/base

(require racket/list
         racket/match
         racket/port
         racket/string
         syntax/strip-context
         peg/peg
         peg/peg-in-peg
         peg/peg-to-scheme
         (only-in peg/s-exp peg-rule:s-exp))

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

(define-peg/tag param-decl (and (drop "{") _ name (* (and (drop ",") _ name)) (drop "}") _))
(define-peg/tag param-ann (and (drop "{") _ ann-name (* (and (drop ",") _ ann-name)) (drop "}") _))
(define-peg/tag ann-name (and (or "+" "~" "?") _ name))
(define-peg/tag ann-rule (and name (? param-decl) (or "<--" "<-" "<") _ ann-pattern (? (and "->" _ s-exp _)) ";" _))
(define-peg/tag ann-pattern (and ann-alternative (* (and SLASH ann-alternative))))
(define-peg/tag ann-alternative (and (? param-ann) (+ ann-expression)))
(define-peg/tag ann-expression (and (? (and name (drop ":") _)) (? (and (or #\! #\& #\~) _)) ann-primary (? (and (or #\* #\+ #\?) _))))
(define-peg/tag ann-primary (or (and "(" _ ann-pattern ")" _) (and "." _) literal charclass (and name (? param-ann))))
(define-peg/tag ann-peg (and _ (* import) (+ ann-rule)))

(define (ann-peg->peg x)
  (match x
    [(list ann-peg (and import (cons 'import _)) ... rule ...)
     `(peg ,@import ,@(append* (map expand-rule rule)))]))

(define (expand-rule x)
  (match x
    [(list-rest 'ann-rule (cons 'name name) (list 'param-decl (cons 'name param) ...) arrow pattern rest)
     (for/list ([variant (in-combinations param)])
       (define vname (string-join (cons name variant) "_"))
       `(rule (name . ,vname) ,arrow ,(expand-pattern pattern variant) . ,rest))]
    [(list-rest 'ann-rule (cons 'name name) arrow pattern rest)
     `((rule (name . ,name) ,arrow ,(expand-pattern pattern '()) . ,rest))]))

(define (expand-pattern x variant)
  (match x
    [(list 'ann-pattern alt ...)
     `(pattern ,@(filter-map (λ (a) (expand-alternative a variant)) alt))]))

(define (expand-alternative x variant)
  (match x
    [(list 'ann-alternative (list 'param-ann (list 'ann-name vop (cons 'name vname)) ...) expr ...)
     (and (for/and ([op (in-list vop)]
                    [name (in-list vname)])
            (cond
              [(and (equal? "+" op) (not (member name variant))) #f]
              [(and (equal? "~" op) (member name variant)) #f]
              [else #t]))
          `(alternative ,@(for/list ([e (in-list expr)])
                            (expand-expression e variant))))]
    [(list 'ann-alternative expr ...)
     `(alternative ,@(for/list ([e (in-list expr)])
                       (expand-expression e variant)))]))

(define (expand-expression x variant)
  (cons
   'expression
   (map (λ (t)
          (match t
            [(cons 'ann-primary _) (expand-primary t variant)]
            [_ t]))
        (cdr x))))

(define (expand-primary x variant)
  (match x
    [(list 'ann-primary (cons 'name name) (list 'param-ann (list 'ann-name vop (cons 'name vname)) ...))
     (define ref-name
       (string-join
        (cons name
              (filter-map
               (λ (op name)
                 (cond
                   [(equal? "+" op) name]
                   [(equal? "?" op) (and (member name variant) name)]
                   [else #f]))
               vop
               vname))
        "_"))
     `(primary name . ,ref-name)]
    [(list 'ann-primary (cons 'name name))
     `(primary name . ,name)]
    [(list 'ann-primary "(" pat ")")
     `(primary "(" ,(expand-pattern pat variant) ")")]
    [(list-rest 'ann-primary rest)
     `(primary . ,rest)]))

(define (ann-peg-port->racket in)
  (peg->scheme
   (ann-peg->peg
    (car (peg (and ann-peg (! (any-char))) (port->string in))))))

(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))

(define (literal-read-syntax src in)
  (with-syntax ([body (ann-peg-port->racket in)])
    (strip-context
     #'(module anything racket
         (provide (all-defined-out))
         (require peg/peg)
         body))))
