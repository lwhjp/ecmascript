#lang scribble/manual
@(require (for-label "../ast.rkt"
                     (only-in "../main.rkt"
                              this
                              return)
                     "../parse.rkt"
                     "../private/statement.rkt"
                     "../types.rkt"))

@title{Parsing ECMAScript}

@defmodule[ecmascript/parse]

@defproc[(read-program
          [source-name any/c (object-name in)]
          [in input-port? (current-input-port)])
         (or/c (listof (or/c function? statement?)) eof-object?)]{
}

@section{Abstract Syntax}

@defmodule[ecmascript/ast]

@defstruct*[syntax-element ([location source-location?])]{
  The base type for all ECMAScript syntax elements.
}

@defstruct*[(identifier syntax-element) ([symbol symbol?])]{
  An identifier.
}

@defstruct*[(function syntax-element)
            ([name (or/c identifier? #f)]
             [parameters (listof identifier?)]
             [body (listof (or/c function? statement?))])]{
  A function definition, which may appear as part of an expression
  (in which case @racket[name] may be @racket[#f]) or program body,
  or nested within another function.
}

@defstruct*[(variable-declaration syntax-element)
            ([name identifier?]
             [initializer (or/c expression? #f)])]{
  A variable declaration with optional initializer.
}

@subsection{Expressions}

@defstruct*[(expression syntax-element) ()]{
  The base type for all ECMAScript expressions.
}

@defstruct*[(expression:literal expression) ([value literal?])]{
  A literal value expression.
}

@defstruct*[(expression:this expression) ()]{
  The @racket[this] keyword.
}

@defstruct*[(expression:reference expression) ([identifier identifier?])]{
  An identifier reference.
}

@defstruct*[(expression:member-reference expression)
            ([base expression?] [property (or/c identifier? expression?)])]{
  A member reference. This struct represents expressions of the form
  @racketfont{object.property} and @racketfont{object[property]},
  depending on the type of @racket[property].
}

@deftogether[(@defstruct*[(operator syntax-element) ([symbol symbol?])]
              @defstruct*[(expression:postfix expression)
                          ([operand expression?] [operator operator?])]
              @defstruct*[(expression:unary expression)
                          ([operator operator?] [operand expression?])]
              @defstruct*[(expression:binary expression)
                          ([left expression?] [operator operator?] [right expression?])])]{
  Operator expressions.
}

@defstruct*[(expression:conditional expression)
            ([test expression?] [true expression?] [false expression?])]{
  The conditional (ternary) operator @racketfont{?:}.
}

@defstruct*[(expression:call expression)
            ([function expression?]
             [arguments (listof expression?)])]{
  A function call.
}

@defstruct*[(expression:new expression)
            ([constructor expression?]
             [arguments (listof expression?)])]{
  A @racket[new] expression.
}

@defstruct*[(expression:function expression) ([definition function?])]{
  A function expression.
}

@defstruct*[(expression:comma expression)
            ([left expression?] [right expression?])]{
  The comma operator.
}

@subsection{Literals}

@defstruct*[(literal syntax-element) ()]{
  The base type for all literals.
}

@defstruct*[(literal:null literal) ()]{
  The @racket[null] keyword.
}

@deftogether[(@defstruct*[(literal:boolean literal) ([value boolean?])]
              @defstruct*[(literal:number literal) ([value number?])]
              @defstruct*[(literal:string literal) ([value string?])])]{
  A boolean, numeric or string literal.
}

@defstruct*[(literal:regexp literal) ([pattern string?] [flags string?])]{
  A regular expression literal. The @racket[flags] field should
  be a (possibly empty) string consisting of a combination of the
  characters @litchar["g"], @litchar["i"] and @litchar["m"].
}

@defstruct*[(literal:array literal)
            ([elements (listof (or/c expression? #f))])]{
  An array literal. A @racket[#f] value for any element indicates
  an elision.
}

@deftogether[(@defstruct*[(literal:object literal)
                          ([properties (listof property-initializer?)])]
              @defstruct*[(property-initializer syntax-element)
                          ([name (or/c identifier? literal:string? literal:number?)])]
              @defstruct*[(property-initializer:data property-initializer)
                          [(value expression?)]]
              @defstruct*[(property-initializer:get property-initializer)
                          [(function function?)]]
              @defstruct*[(property-initializer:set property-initializer)
                          [(function function?)]])]{
  An object literal.

  A @racket[property-initializer:get] initializer must be an anonymous
  function of no arguments. A @racket[property-initializer:set]
  initializer must be an anonymous function of one argument.
}

@subsection{Statements}

@defstruct*[(statement syntax-element) ()]{
  The base type for all statements.
}

@defstruct*[(statement:block statement) ([body (listof statement?)])]{
  A @racket[block].
}

@defstruct*[(statement:break statement) ([label (or/c identifier? #f)])]{
  A @racket[break] statement.
}

@defstruct*[(statement:continue statement) ([label (or/c identifier? #f)])]{
  A @racket[continue] statement.
}

@defstruct*[(statement:debugger statement) ()]{
  A @racket[debugger] statement.
}

@defstruct*[(statement:do statement) ([body statement?] [test expression?])]{
  A @racket[do] statement.
}

@defstruct*[(statement:empty statement) ()]{
  An empty statement. See @racket[empty-statement].
}

@defstruct*[(statement:expression statement) ([expression expression?])]{
  An expression statement.
}

@defstruct*[(statement:for statement)
            ([initializer (or/c expression? (listof variable-declaration?) #f)]
             [test (or/c expression? #f)]
             [update (or/c expression? #f)]
             [body statement?])]{
  A @racket[for] statement.
}

@defstruct*[(statement:for-in stmt)
            ([index (or/c expression? variable-declaration?)]
             [expression expression?]
             [body statement?])]{
  See @racket[for-in].
}

@defstruct*[(statement:if statement)
            ([test expression?]
             [true statement?]
             [false (or/c statement? #f)])]{
  An @racket[if] statement.
}

@defstruct*[(statement:label statement)
            ([label identifier?]
             [statement statement?])]{
  A labelled statement. See @racket[label].
}

@defstruct*[(statement:return statement)
            ([expression (or/c expression? #f)])]{
  A @racket[return] statement.
}

@deftogether[(@defstruct*[(statement:switch statement)
                          ([expression expression?]
                           [body (listof (or/c case-clause? default-clause?))])]
              @defstruct*[(case-clause syntax-element)
                          ([expression expression?] [body (listof statement?)])]
              @defstruct*[(default-clause syntax-element)
                          ([body (listof statement?)])])]{
  A @racket[switch] statement.
}

@defstruct*[(statement:throw statement) ([expression expression?])]{
  A @racket[throw] statement.
}

@defstruct*[(statement:try statement)
            ([body statement:block?]
             [catch-id (or/c identifier? #f)]
             [catch-body (or/c statement:block? #f)]
             [finally-body (or/c statement:block? #f)])]{
  A @racket[try] statement.
}

@defstruct*[(statement:var statement)
            ([declarations (listof variable-declaration?)])]{
  A variable declaration statement.
}

@defstruct*[(statement:while statement)
            ([test expression?] [body statement?])]{
  A @racket[while] statement.
}

@defstruct*[(statement:with statement)
            ([expression expression?] [body statement?])]{
  A @racket[with] statement.
}
