#lang racket/base

(require racket/list
         racket/match
         syntax/strip-context
         "syntax.rkt")

(provide es-compile)

(define (es-compile stx)
  (map
   strip-context
   (compile-program stx)))

(define (extract-id stx)
  (match stx
    [(es-id stx name) (quasisyntax/loc stx #,name)]))

(define (extract-statement-vars stmt)
  (match stmt
    [(es-var-stmt _ (list (es-var-decl _ (es-id _ name) _) ...))
     name]
    [_ #f]))

(define (extract-block-vars block)
  (apply
   append
   (filter-map
    extract-statement-vars
    (es-block-statements block))))

(define (extract-program-vars prog)
  (apply
   append
   (filter-map
    extract-statement-vars
    (es-program-elements prog))))

(define (extract-functions prog)
  (filter-map
   (λ (elt)
     (and (es-function? elt) elt))
   (es-program-elements prog)))

(define (add-prefix pref sym)
  (string->symbol
   (string-append (symbol->string pref)
                  (symbol->string sym))))

(define (compile-expression stx)
  (match stx
    [(es-this stx) (quasisyntax/loc stx this)]
    [(es-id stx name) (quasisyntax/loc stx (id #,name))]
    [(es-numeric stx v) (quasisyntax/loc stx #,v)]
    [(es-string stx v) (quasisyntax/loc stx #,v)]
    [(es-null stx) (quasisyntax/loc stx null)]
    [(es-bool stx v) (quasisyntax/loc stx v)]
    [(es-member-expr stx obj (? es-id? prop))
     (quasisyntax/loc stx (member #,(compile-expression obj)
                                  #,(symbol->string (es-id-name prop))))]
    [(es-member-expr stx obj prop)
     (quasisyntax/loc stx (member #,(compile-expression obj)
                                  #,(compile-expression prop)))]
    [(es-new-expr stx expr args)
     (quasisyntax/loc stx (new #,(compile-expression expr)
                               #,@(map compile-expression args)))]
    [(es-call-expr stx expr args)
     (quasisyntax/loc stx (call #,(compile-expression expr)
                                #,@(map compile-expression args)))]
    [(es-postfix-expr stx expr op)
     (quasisyntax/loc stx (#,(add-prefix 'post op)
                           #,(compile-expression expr)))]
    [(es-unary-expr stx expr op)
     (quasisyntax/loc stx (#,op #,(compile-expression expr)))]
    [(es-binary-expr stx left op right)
     (quasisyntax/loc stx (#,op #,(compile-expression left)
                                #,(compile-expression right)))]
    [(es-ternary-expr stx test true false)
     (quasisyntax/loc stx (?: #,(compile-expression test)
                              #,(compile-expression true)
                              #,(compile-expression false)))]
    [(es-comma-expr stx left right)
     (quasisyntax/loc stx (\, #,(compile-expression left)
                              #,(compile-expression right)))]))

(define (compile-statement stx)
  (match stx
    [(es-block stx stmts)
     (quasisyntax/loc stx (block #,@(map compile-statement stmts)))]
    [(es-var-stmt stx (list (es-var-decl dstx id expr) ...))
     (let ([pairs (filter cdr (map cons id expr))])
       (if (null? pairs)
           (quasisyntax/loc stx (empty-statement))
           (quasisyntax/loc stx
             (block
              #,@(map (λ (decl)
                        #`(put-value
                           (id #,(extract-id (car decl)))
                           #,(compile-expression (cdr decl))))
                      pairs)))))]
    [(es-empty-stmt stx)
     (quasisyntax/loc stx (empty-statement))]
    [(es-expr-stmt stx expr)
     (quasisyntax/loc stx (get-value #,(compile-expression expr)))]
    [(es-if stx test true false)
     (if false
         (quasisyntax/loc stx
           (if #,(compile-expression test)
               #,(compile-statement true)
               #,(compile-statement false)))
         (quasisyntax/loc stx
           (if #,(compile-expression test)
               #,(compile-statement true))))]
    [(es-while stx test body)
     (quasisyntax/loc stx
       (while #,(compile-expression test)
              #,(compile-statement body)))]
    [(es-for stx (list var-decl ...) test step body)
     (error 'TODO-vars)]
    [(es-for stx init test step body)
     (quasisyntax/loc stx
       (for #,@(if init
                   #`(#:init #,(compile-expression init))
                   '())
            #,@(if test
                   #`(#:test #,(compile-expression test))
                   '())
            #,@(if step
                   #`(#:step #,(compile-expression step))
                   '())
         #,(compile-statement body)))]
    [(es-for-in stx init expr body)
     (error 'TODO-for-in)]
    [(es-continue stx) (quasisyntax/loc stx (continue))]
    [(es-break stx) (quasisyntax/loc stx (break))]
    [(es-return stx expr)
     (if expr
         (quasisyntax/loc stx (return #,(compile-expression expr)))
         (quasisyntax/loc stx (return)))]
    [(es-with stx expr body)
     (quasisyntax/loc stx
       (with #,(compile-expression expr)
             #,@(map compile-expression body)))]))

(define (compile-function stx)
  (match stx
    [(es-function stx _ args body)
     (let ([vars (extract-block-vars body)])
       (quasisyntax/loc stx
         (function #,(map extract-id args)
           (declare-vars #,@vars)
           #,@(map compile-statement (es-block-statements body)))))]))

(define (compile-source-element stx)
  (if (es-function? stx)
      (compile-function stx)
      (compile-statement stx)))

(define (compile-program prog)
  (match prog
    [(es-program stx elts)
     (let ([fns (extract-functions prog)]
           [vars (extract-program-vars prog)])
       (append
        (list #`(declare-vars #,@vars))
        (map
         (λ (fn)
           #`(put! global-object
                   #,(symbol->string
                      (syntax-e
                       (extract-id
                        (es-function-name fn))))
                   #,(compile-function fn)))
         fns)
        (filter-map
         (λ (elt)
           (and (not (es-function? elt))
                (compile-statement elt)))
         elts)))]))
