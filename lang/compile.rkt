#lang racket/base

(require racket/contract
         racket/list
         racket/match
         racket/syntax
         syntax/strip-context
         "../ast.rkt")

(provide/contract
 [ecmascript->racket
  (-> (listof (or/c declaration? statement?))
      syntax?)])

(define (ecmascript->racket script)
  (strip-context
   ; TODO: check for strict mode
   (with-syntax ([(stmt ...) (map compile-statement script)]
                 [(var ...) (extract-var-names* script)])
     #'(#:vars (var ...)
        stmt ...))))

(define (compile-statement stmt)
  (let c ([node stmt])
    (define stx-e
      (match node
        [(case-clause _ expr body)
         `((,(c expr) ,(c body)))]
        [(declaration:const _ decls)
         `(const ,@(map c decls))]
        [(declaration:function _ body)
         (c body)]
        [(declaration:let _ decls)
         `(let ,@(map c decls))]
        [(declaration:var _ decls)
         `(var ,@(map c decls))]
        [(default-clause _ body)
         `(default ,(c body))]
        [(expression:binary _ l op r)
         `(,(c op) ,(c l) ,(c r))]
        [(expression:call _ f args)
         `(call ,(c f) ,@(map c args))]
        [(expression:comma _ l r)
         `(\, ,(c l) ,(c r))]
        [(expression:conditional _ t e1 e2)
         `(?: ,(c e1) ,(c e2))]
        [(expression:function _ f)
         (c f)]
        [(expression:literal _ v)
         (c v)]
        [(expression:member-reference _ b p)
         `(member ,(c b) ,(c p))]
        [(expression:new _ t args)
         `(new ,(c t) ,@(map c args))]
        [(expression:postfix _ e op)
         `(,(postfix (c op)) ,(c e))]
        [(expression:reference _ id)
         `(identifier ,(c id))]
        [(expression:super-call _ args)
         `(super ,@(map c args))]
        [(expression:super-reference _ p)
         `(member super ,(c p))]
        [(expression:this _)
         'this]
        [(expression:unary _ op e)
         `(,(c op) ,(c e))]
        [(function _ name parameters body)
         `(function ,@(if name (list (c name)) '())
                    ,(map c parameters)
                    #:vars ,(extract-var-names* body)
            ,@(map c body))]
        [(identifier _ s) s]
        [(literal:array _ es) `(array ,@(map c es))]
        [(literal:boolean _ v) v]
        [(literal:null _) 'null]
        [(literal:number _ v) (exact->inexact v)]
        [(literal:object _ props) `(object ,@(map c props))]
        [(literal:regexp _ pattern flags)
         `(regexp ,pattern ,(list->string flags))]
        [(literal:string _ v) v]
        [(operator _ s)
         (string->symbol s)]
        [(property-initializer:data _ n e) `[,n ,(c e)]]
        [(property-initializer:get _ n f) `[,n #:get ,(c f)]]
        [(property-initializer:set _ n f) `[,n #:set ,(c f)]]
        [(statement:block _ body)
         `(block #:vars ,(append* (map extract-lexical-var-names body))
            ,@(map c body))]
        [(statement:break _ label)
         `(break ,@(if label (list (c label)) '()))]
        [(statement:continue _ label)
         `(continue ,@(if label (list (c label)) '()))]
        [(statement:debugger _)
         `(debugger)]
        [(statement:do _ body test)
         `(do ,(c body) ,(c test))]
        [(statement:empty _)
         `(empty-statement)]
        [(statement:expression _ e) (c e)]
        [(statement:for _ init test update body)
         `(for #:init ,(and init (c init))
               #:test ,(and test (c test))
               #:update ,(and update (c update))
            ,(c body))]
        [(statement:for-in _ index expr body)
         `(for-in ,(c index) ,(c expr) ,(c body))]
        [(statement:if _ test e1 e2)
         `(if ,(c test) ,(c e1) ,@(if e2 (list (c e2)) '()))]
        [(statement:label _ l s)
         `(label ,(c l) ,(c s))]
        [(statement:return _ e)
         `(return ,@(if e (list (c e)) '()))]
        [(statement:switch _ expr body)
         `(switch ,(c expr) ,@(map c body))]
        [(statement:throw _ e)
         `(throw ,(c e))]
        [(statement:try _ body cid cb fb)
         `(try ,@(map c body)
               ,@(if cid `(#:catch ,(c cid) ,@(map c cb)) '())
               ,@(if fb `(#:finally ,@(map c fb)) '()))]
        [(statement:while _ test body)
         `(while ,(c test) ,(c body))]
        [(statement:with _ expr body)
         `(with ,(c expr) ,(c body))]
        [(variable-declaration _ id expr)
         (if expr (list (c id) (c expr)) (c id))]))
    (datum->syntax #f stx-e (syntax-element-location node))))

(define (extract-var-names* nodes)
  (remove-duplicates
   (append*
    (append
     (map extract-lexical-var-names nodes)
     (map extract-var-names nodes)))))

(define (extract-lexical-var-names node)
  (match node
    [(declaration:const _ (list (variable-declaration _ binding _) ...))
     (append* (map extract-var-names/binding binding))]
    [(declaration:let _ (list (variable-declaration _ binding _) ...))
     (append* (map extract-var-names/binding binding))]
    [_ '()]))

(define (extract-var-names node)
  (remove-duplicates
   (let loop ([node node])
     (match node
       [(declaration:var _ (list (variable-declaration _ binding _) ...))
        (append* (map extract-var-names/binding binding))]
       [(statement:block _ stmts)
        (append* (map loop stmts))]
       [(statement:if _ _ true false)
        (append (loop true)
                (loop false))]
       [(statement:do _ body _)
        (loop body)]
       [(statement:while _ _ body)
        (loop body)]
       [(statement:for _ (declaration:var _ (list (variable-declaration _ id _) ...)) _ _ body)
        (append (append* (map extract-var-names/binding id))
                (loop body))]
       [(statement:for _ _ _ _ body)
        (loop body)]
       [(statement:for-in _ (variable-declaration _ id _) _ body)
        (append (extract-var-names/binding id)
                (loop body))]
       [(statement:with _ _ body)
        (loop body)]
       [(statement:switch _ _ cases)
        (append* (map loop cases))]
       [(statement:label _ _ stmt)
        (loop stmt)]
       [(statement:try _ block _ catch finally)
        (append (loop block)
                (loop catch)
                (loop finally))]
       [_ '()]))))

(define extract-var-names/binding
  (match-lambda
    [(identifier _ symbol) (list symbol)]))

(define (postfix s)
  (format-id s "post~a" s))
