#lang scribble/manual
@(require (for-label racket/base
                     racket/contract
                     syntax/srcloc
                     "../ast.rkt"))

@title{ECMAScript Syntax}

@margin-note{
  @bold{Warning}
  These definitions are not fixed and are likely to change
  in the future.
}

@section{Abstract Syntax}

@defmodule[ecmascript/ast]

@defstruct*[syntax-element ([loc source-location?])]{
}

@defstruct*[(source-element syntax-element) ()]{
  The top-level elements which make up programs and function bodies.
}

@subsection{Expressions}

@defstruct*[(expr syntax-element) ()]{
  The base type for ECMAScript expressions.
}

@deftogether[(@defstruct*[(expr:this expr) ()]
              @defstruct*[(expr:null expr) ()])]{
  The @racketidfont{this} and @racketvalfont{null} keywords.
}

@defstruct*[(expr:id expr) ([symbol symbol?])]{
  An identifier.
}

@deftogether[(@defstruct*[(expr:bool expr) ([value boolean?])]
              @defstruct*[(expr:number expr) ([value number?])]
              @defstruct*[(expr:string expr) ([value string?])])]{
  A literal value.
}

@defstruct*[(expr:regexp expr) ([pattern string?] [flags string?])]{
  A regular expression literal. The @racket[flags] field should
  be a (possibly empty) string consisting of a combination of the
  characters @litchar["g"], @litchar["i"] and @litchar["m"].
}

@defstruct*[(expr:array expr) ([elements (listof expr?)])]{
  An array literal.
}

@defstruct*[(expr:object expr)
            ([properties (listof (or/c init:obj:prop?
                                       init:obj:get?
                                       init:obj:set?))])]{
  An object literal.
}

@defstruct*[(expr:fn expr) ([def fn?])]{
  A function expression.
}

@defstruct*[(expr:member expr) ([object expr?] [property expr?])]{
  A member reference. This struct represents expressions of the form
  @racketfont{_object._property} and @racketfont{_object[_property]},
  depending on the type of @racketidfont{property}.
}

@defstruct*[(expr:new expr) ([ctor expr?] [args (listof expr?)])]{
  A @racketkeywordfont{new} expression.
}

@defstruct*[(expr:call expr) ([fn expr?] [args (listof expr?)])]{
  A function call.
}

@deftogether[(@defstruct*[(expr:postfix expr) ([expr expr?] [op symbol?])]
              @defstruct*[(expr:unary expr) ([op symbol?] [expr expr?])]
              @defstruct*[(expr:binary expr) ([left expr?] [op symbol?] [right expr?])])]{
  Operator expressions.
}

@defstruct*[(expr:cond expr) ([test expr?] [true expr?] [false expr?])]{
  The conditional (ternary) operator @racketfont{?:}.
}

@defstruct*[(expr:comma expr) ([left expr?] [right expr?])]{
  The comma operator.
}

@subsection{Initializers}

@defstruct*[(init syntax-element) ()]{
}

@deftogether[(@defstruct*[(init:obj:prop init)
                          ([name (or/c symbol? string? number?)]
                           [value expr?])]
              @defstruct*[(init:obj:get init)
                          ([prop (or/c symbol? string? number?)]
                           [fn fn?])]
              @defstruct*[(init:obj:set init)
                          ([prop (or/c symbol? string? number?)]
                           [fn fn?])])]{
  Property initializers for object literals. The argument for setter
  functions is defined in the @racketidfont{fn} field.
}

@subsection{Declarations}

@defstruct*[(decl:fn source-element) ([def fn?])]{
  A function declaration.
}

@defstruct*[(decl:var syntax-element) ([id symbol?] [expr (or/c expr? #f)])]{
  A variable declaration.
}

@subsection{Statements}

@defstruct*[(stmt source-element) ()]{
}

@defstruct*[(stmt:block stmt) ([stmts (listof stmt?)])]{
  A block.
}

@defstruct*[(stmt:vars stmt) ([decls (listof decl:var?)])]{
  A @racketkeywordfont{vars} statement.
}

@defstruct*[(stmt:empty stmt) ()]{
  An empty statement.
}

@defstruct*[(stmt:if stmt) ([test expr?] [true stmt?] [false (or/c stmt? #f)])]{
  An @racketkeywordfont{if} statement.
}

@defstruct*[(stmt:do stmt) ([body stmt?] [test expr?])]{
  A @racketkeywordfont{do} statement.
}

@defstruct*[(stmt:while stmt) ([test expr?] [body stmt?])]{
  A @racketkeywordfont{while} statement.
}

@;@defstruct*[(stmt:for stmt) ([init] [test] [update])]{}

@;@defstruct*[(stmt:for-in stmt) (i expr body)]{}

@defstruct*[(stmt:continue stmt) ([label (or/c symbol? #f)])]{
  A @racketkeywordfont{continue} statement.
}

@defstruct*[(stmt:break stmt) ([label (or/c symbol? #f)])]{
  A @racketkeywordfont{break} statement.
}

@defstruct*[(stmt:return stmt) ([expr (or/c expr? #f)])]{
  A @racketkeywordfont{return} statement.
}

@defstruct*[(stmt:with stmt) ([expr expr?] [body stmt?])]{
  A @racketkeywordfont{with} statement.
}

@;@defstruct*[(stmt:switch stmt) (expr cases)]{}

@defstruct*[(stmt:labelled stmt) ([label symbol?] [stmt stmt?])]{}

@defstruct*[(stmt:throw stmt) ([expr expr?])]{
  A @racketkeywordfont{throw} statement.
}

@;@defstruct*[(stmt:try stmt) (block catch-id catch finally)]{}

@defstruct*[(stmt:debugger stmt) ()]{}

@subsection{Function definitions}

@defstruct*[(fn syntax-element)
            ([name symbol?]
             [params (listof symbol?)]
             [body (listof source-element?)])]{
}
