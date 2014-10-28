#lang racket/base

(provide (all-defined-out))

(struct syntax-element (location) #:transparent)

(struct case-clause syntax-element (expression body) #:transparent)

(struct default-clause syntax-element (body) #:transparent)

(struct expression syntax-element () #:transparent)
(struct expression:binary expression (left operator right) #:transparent)
(struct expression:call expression (function arguments) #:transparent)
(struct expression:comma expression (left right) #:transparent)
(struct expression:conditional expression (test true false) #:transparent)
(struct expression:function expression (definition) #:transparent)
(struct expression:literal expression (value) #:transparent)
(struct expression:member-reference expression (base property) #:transparent)
(struct expression:new expression (constructor arguments) #:transparent)
(struct expression:postfix expression (operand operator) #:transparent)
(struct expression:reference expression (identifier) #:transparent)
(struct expression:this expression () #:transparent)
(struct expression:unary expression (operator operand) #:transparent)

(struct function syntax-element (name parameters body) #:transparent)

(struct identifier syntax-element (symbol) #:transparent)

(struct literal syntax-element () #:transparent)
(struct literal:array literal (elements) #:transparent)
(struct literal:boolean literal (value) #:transparent)
(struct literal:null literal () #:transparent)
(struct literal:number literal (value) #:transparent)
(struct literal:object literal (properties) #:transparent)
(struct literal:regexp literal (pattern flags) #:transparent)
(struct literal:string literal (value) #:transparent)

(struct operator syntax-element (symbol) #:transparent)

(struct property-initializer syntax-element (name) #:transparent)
(struct property-initializer:data property-initializer (value) #:transparent)
(struct property-initializer:get property-initializer (function) #:transparent)
(struct property-initializer:set property-initializer (function) #:transparent)

(struct statement syntax-element () #:transparent)
(struct statement:block statement (body) #:transparent)
(struct statement:break statement (label) #:transparent)
(struct statement:continue statement (label) #:transparent)
(struct statement:debugger statement () #:transparent)
(struct statement:do statement (body test) #:transparent)
(struct statement:empty statement () #:transparent)
(struct statement:expression statement (expression) #:transparent)
(struct statement:for statement (initializer test update body) #:transparent)
(struct statement:for-in statement (index expression body) #:transparent)
(struct statement:if statement (test true false) #:transparent)
(struct statement:label statement (label statement) #:transparent)
(struct statement:return statement (expression) #:transparent)
(struct statement:switch statement (expression body) #:transparent)
(struct statement:throw statement (expression) #:transparent)
(struct statement:try statement (body catch-id catch-body finally-body) #:transparent)
(struct statement:var statement (declarations) #:transparent)
(struct statement:while statement (test body) #:transparent)
(struct statement:with statement (expression body) #:transparent)

(struct variable-declaration syntax-element (name initializer) #:transparent)
