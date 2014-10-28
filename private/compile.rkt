#lang racket/base

(require racket/contract
         racket/list
         racket/match
         (prefix-in ast: "../ast.rkt"))

(provide/contract
 [ecmascript->racket
  (-> (listof (or/c ast:function? ast:statement?))
      (listof syntax?))])

(define (ecmascript->racket stx)
  (list (compile-program stx)))

(define (extract-statement-vars stmt)
  (match stmt
    [(ast:statement:block _ stmts)
     (apply append (map extract-statement-vars stmts))]
    [(ast:statement:var _ (list (ast:variable-declaration _ id _) ...))
     id]
    [(ast:statement:if _ _ true false)
     (append (extract-statement-vars true)
             (extract-statement-vars false))]
    [(ast:statement:do _ body _)
     (extract-statement-vars body)]
    [(ast:statement:while _ _ body)
     (extract-statement-vars body)]
    [(ast:statement:for _ (list (ast:variable-declaration _ id _) ...) _ _ body)
     (append id (extract-statement-vars body))]
    [(ast:statement:for _ _ _ _ body)
     (extract-statement-vars body)]
    [(ast:statement:for-in _ (ast:variable-declaration _ i _) _ body)
     (cons i (extract-statement-vars body))]
    [(ast:statement:with _ _ body)
     (extract-statement-vars body)]
    [(ast:statement:switch _ _ cases)
     (apply append (map extract-statement-vars cases))]
    [(ast:statement:label _ _ stmt)
     (extract-statement-vars stmt)]
    [(ast:statement:try _ block _ catch finally)
     (append (extract-statement-vars block)
             (extract-statement-vars catch)
             (extract-statement-vars finally))]
    [_ '()]))

(define (extract-vars elts)
  (apply
   append
   (map
    extract-statement-vars
    elts)))

(define (compile-identifier id)
  (match-define (ast:identifier loc sym) id)
  (datum->syntax #f sym loc))

(define (add-prefix pref sym)
  (string->symbol
   (string-append (symbol->string pref)
                  (symbol->string sym))))

(define (compile-expression stx)
  (match stx
    [(ast:expression:this loc)
     (datum->syntax #f 'this loc)]
    [(ast:expression:reference loc id)
     (datum->syntax #f `(id ,(compile-identifier id)) loc)]
    [(ast:expression:literal _ (ast:literal:null loc))
     (datum->syntax #f 'null loc)]
    [(ast:expression:literal _ (ast:literal:boolean loc v))
     (datum->syntax #f v loc)]
    [(ast:expression:literal _ (ast:literal:number loc v))
     (datum->syntax #f v loc)]
    [(ast:expression:literal _ (ast:literal:string loc v))
     (datum->syntax #f v loc)]
    [(ast:expression:literal _ (ast:literal:regexp loc pattern flags))
     (datum->syntax #f `(regexp ,pattern ,flags) loc)]
    [(ast:expression:literal _ (ast:literal:array loc elements))
     (datum->syntax #f
       `(array
         ,@(map (λ (elt)
                  (if elt
                      (compile-expression elt)
                      'undefined))
                elements))
       loc)]
    [(ast:expression:literal _ (ast:literal:object loc props))
     (datum->syntax #f
       `(object
         ,@(map compile-object-property props))
       loc)]
    [(ast:expression:function _ fn) (compile-function fn)]
    [(ast:expression:member-reference loc obj prop)
     (datum->syntax #f
       `(member ,(compile-expression obj)
                ,(if (ast:identifier? prop)
                     (compile-identifier prop)
                     (compile-expression prop)))
       loc)]
    [(ast:expression:new loc expr args)
     (datum->syntax #f
       `(new ,(compile-expression expr)
             ,@(map compile-expression args))
       loc)]
    [(ast:expression:call loc expr args)
     (datum->syntax #f
       `(call ,(compile-expression expr)
              ,@(map compile-expression args))
       loc)]
    [(ast:expression:postfix loc expr op)
     (datum->syntax #f
       `(,(datum->syntax #f
            (add-prefix 'post (ast:operator-symbol op))
            (ast:syntax-element-location op))
         ,(compile-expression expr))
       loc)]
    [(ast:expression:unary loc op expr)
     (datum->syntax #f
       `(,(datum->syntax #f
            (ast:operator-symbol op)
            (ast:syntax-element-location op))
         ,(compile-expression expr))
       loc)]
    [(ast:expression:binary loc left op right)
     (datum->syntax #f
       `(,(datum->syntax #f
            (ast:operator-symbol op)
            (ast:syntax-element-location op))
         ,(compile-expression left)
         ,(compile-expression right))
       loc)]
    [(ast:expression:conditional loc test true false)
     (datum->syntax #f
       `(?: ,(compile-expression test)
            ,(compile-expression true)
            ,(compile-expression false))
       loc)]
    [(ast:expression:comma loc left right)
     (datum->syntax #f
       `(\, ,(compile-expression left)
            ,(compile-expression right))
       loc)]))

(define (compile-object-property stx)
  (define name-stx
    (match (ast:property-initializer-name stx)
      [(ast:identifier loc sym) (datum->syntax #f sym loc)]
      [(ast:literal:string loc v) (datum->syntax #f v loc)]
      [(ast:literal:number loc v) (datum->syntax #f v loc)]))
  (match stx
    [(ast:property-initializer:data loc _ value)
     (datum->syntax #f
       `[,name-stx ,(compile-expression value)]
       loc)]
    [(ast:property-initializer:get loc _ fn)
     (datum->syntax #f
       `[,name-stx #:get ,(compile-function fn)]
       loc)]
    [(ast:property-initializer:set loc _ fn)
     (datum->syntax #f
       `[,name-stx #:set ,(compile-function fn)]
       loc)]))

(define (compile-statement stx)
  (match stx
    [(ast:statement:block loc stmts)
     (datum->syntax #f
       `(block ,@(map compile-statement stmts))
       loc)]
    [(ast:statement:var loc decls)
     (compile-variable-declaration-list decls loc)]
    [(ast:statement:empty loc)
     (datum->syntax #f
       '(empty-statement)
       loc)]
    [(ast:statement:expression loc expr)
     (datum->syntax #f
       `(get-value ,(compile-expression expr))
       loc)]
    [(ast:statement:if loc test true false)
     (datum->syntax #f
       (if false
           `(if ,(compile-expression test)
                ,(compile-statement true)
                ,(compile-statement false))
           `(if ,(compile-expression test)
                ,(compile-statement true)))
       loc)]
    [(ast:statement:while loc test body)
     (datum->syntax #f
       `(while ,(compile-expression test)
          ,(compile-statement body))
       loc)]
    [(ast:statement:for loc (list var-decl ...) test update body)
     (datum->syntax #f
       `(for #:init ,(compile-variable-declaration-list var-decl loc)
             #:test ,(and test
                          (compile-expression test))
             #:update ,(and update
                            (compile-expression update))
          ,(compile-statement body)))]
    [(ast:statement:for loc init test update body)
     (datum->syntax #f
       `(for #:init ,(and init
                          (compile-expression init))
             #:test ,(and test
                          (compile-expression test))
             #:update ,(and update
                            (compile-expression update))
          ,(compile-statement body)))]
    [(ast:statement:for-in loc (ast:variable-declaration _ vid vexpr) expr body)
     (datum->syntax #f
       `(for-in (id ,(compile-identifier vid))
                ,(compile-expression expr)
          ,(compile-statement body)))]
    [(ast:statement:for-in loc lhs expr body)
     (datum->syntax #f
       `(for-in ,(compile-expression lhs) ,(compile-expression expr)
          ,(compile-statement body)))]
    [(ast:statement:continue loc label)
     (datum->syntax #f
       (if label
           `(continue ,(compile-identifier label))
           '(continue))
       loc)]
    [(ast:statement:break loc label)
     (datum->syntax #f
       (if label
           `(break ,(compile-identifier label))
           '(break)))]
    [(ast:statement:return loc expr)
     (datum->syntax #f
       (if expr
           `(return ,(compile-expression expr))
           '(return))
       loc)]
    [(ast:statement:with loc expr body)
     (datum->syntax #f
       `(with ,(compile-expression expr)
          ,(compile-statement body))
       loc)]
    [(ast:statement:label loc label stmt)
     (datum->syntax #f
       `(label ,(compile-identifier label)
          ,(compile-statement stmt))
       loc)]
    [(ast:statement:throw loc expr)
     (datum->syntax #f
       `(throw ,(compile-expression expr))
       loc)]
    [(ast:statement:try loc tb cid cb fb)
     (datum->syntax #f
       `(try ,(compile-statement tb)
             ,@(if cid
                   `(#:catch ,(compile-identifier cid)
                             ,(compile-statement cb))
                   '())
             ,@(if fb `(#:finally ,(compile-statement fb)) '()))
       loc)]))

(define (compile-variable-declaration-list stx loc)
  (let ([assignments
         (filter-map
          (λ (decl)
            (match-define (ast:variable-declaration loc id expr) decl)
            (and expr
                 (datum->syntax #f
                   `(put-value!
                     (id ,(compile-identifier id))
                     (get-value ,(compile-expression expr)))
                   loc)))
          stx)])
    (datum->syntax #f
      (if (null? assignments)
          '(empty-statement)
          `(block ,@assignments))
      loc)))

(define (compile-body elts)
  (map
   (match-lambda
     [(? ast:statement? stmt) (compile-statement stmt)]
     [(? ast:function? fn) (compile-function fn)])
   elts))

(define (compile-function stx)
  (match-define (ast:function loc name params body) stx)
  (let ([vars (extract-vars body)]
        [compiled-body (compile-body body)])
    (datum->syntax #f
      (if name
          `(function ,(compile-identifier name)
                     ,(map compile-identifier params)
                     #:vars ,(map compile-identifier vars)
             ,@compiled-body)
          `(function ,(map compile-identifier params)
                     #:vars ,(map compile-identifier vars)
             ,@compiled-body)))))

(define (compile-program prog)
  (datum->syntax #f
    `(begin-scope (new-object-environment global-object lexical-environment)
       #:vars ,(map compile-identifier (extract-vars prog))
       ,@(compile-body prog))))
