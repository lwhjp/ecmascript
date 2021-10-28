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
                 [(var ...) (extract-var-names script)])
     #'(begin-scope (new-object-environment (current-global-object) lexical-environment)
         #:vars (var ...)
         stmt ...))))

(define (compile-statement stmt)
  (let c ([node stmt])
    (define stx-e
      (match node
        [(case-clause _ expr body)
         `((,(c expr) ,(c body)))]
        [(declaration:function _ body)
         (c body)]
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
         `(id ,(c id))]
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
                    #:vars ,(append* (map extract-var-names body))
            ,@(map c body))]
        [(identifier _ s)
         (parse-identifier s)]
        [(literal:array _ es)
         `(array ,@(map c es))]
        [(literal:boolean _ v)
         v]
        [(literal:null _)
         'null]
        [(literal:number _ v)
         (parse-number v)]
        ; object
        [(literal:regexp _ pattern flags)
         `(regexp ,pattern ,(list->string flags))]
        [(literal:string _ v)
         (parse-string v)]
        [(operator _ s)
         (string->symbol s)]
        [(statement:block _ body)
         `(block ,@(map c body))]
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
        [(statement:expression _ e)
         `(get-value ,(c e))]
        [(statement:for _ init test update body)
         `(for #:init ,(c init)
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
       [(statement:for _ (list (variable-declaration _ id _) ...) _ _ body)
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
    [(identifier _ symbol) (list (parse-identifier symbol))]))

(define (parse-number s)
  (cond
    [(regexp-match? #rx"^0[xX]" s)
     (string->number (substring s 2) 16)]
    [else (string->number s)]))

(define (parse-string s)
  (define (parse-escape esc)
    (define c (string-ref esc 1))
    (case c
      [(#\u #\x)
       (string
        (integer->char
         (string->number
          (substring esc 2)
          16)))]
      [(#\u000A #\u000D #\u2028 #\u2029) ""]
      [else
       (hash-ref
        #hasheqv((#\b . "\u0008")
                 (#\t . "\u0009")
                 (#\n . "\u000A")
                 (#\v . "\u000B")
                 (#\f . "\u000C")
                 (#\r . "\u000D"))
        c
        (string c))]))
  (define escapes
    (regexp-match-positions*
     #px"\\\\([^ux]|x[[:xdigit:]]{2}|u[[:xdigit:]]{4})"
     s))
  (let loop ([begin 0]
             [escapes escapes])
    (if (null? escapes)
        (substring s begin)
        (string-append
         (substring s begin (caar escapes))
         (parse-escape (substring s (caar escapes) (cdar escapes)))
         (loop (cdar escapes) (cdr escapes))))))

(define (parse-identifier s)
  (string->symbol
   (regexp-replace*
    #rx"\\\\u({([^}]+)}|(....))"
    s
    (λ (a b c d)
      (integer->char
       (string->number (or c d) 16))))))

(define (postfix s)
  (format-id s "post~a" s))
