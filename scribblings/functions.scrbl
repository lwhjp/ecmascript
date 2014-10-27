#lang scribble/manual
@(require (for-label "../function.rkt"
                     "../types.rkt"))

@title{Functions}

@declare-exporting[ecmascript/function]

@defform[(function maybe-id (param-id ...) maybe-vars body ...+)
         #:grammar
         [(maybe-id (code:line) (code:line id))
          (maybe-vars (code:line) (code:line #:vars (var-id ...)))]]{
  Produces an ECMAScript function. Each parameter @racket[param-id]
  is bound as a parameter in the ECMAScript environment, but is not
  bound as a Racket variable.

  If @racket[id] is provided, it is the name of the function.

  If @racket[maybe-vars] is provided, each @racket[var-id] is
  initialized as an ECMAScript variable binding.
}

@defform*[((return)
           (return expr))]{
  Return early from the enclosing @racket[function]. If @racket[expr]
  is provided it will be the return value. This form is illegal outside
  of a function body.
}

@defidform[this]{
  Binds to the ECMAScript @racketfont{this} value for the current
  execution context.
}

@defproc[(call [ref function?] [arg value?] ...) (or/c value? void?)]{
  Invokes the function from @racket[ref]. Each @racket[arg] is bound
  to a formal parameter of the function in order. Surplus arguments
  are ignored, and if insufficient arguments are passed the remaining
  parameters will be @racket[undefined].

  The @racket[this] value will be bound to the base of @racket[ref]
  if it is a reference, or to the function itself otherwise.
}

@defproc[(new [ref constructor?] [arg value?] ...) object?]{
  Invokes @racket[ref] as a constructor, with each @racket[arg]
  bound to a formal parameter. Surplus arguments are ignored, and
  any remaining parameters are bound to @racket[undefined].
}

@section{Function Predicates}

@defmodule[ecmascript/function]

@defproc[(function? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is an ECMAScript function,
  @racket[#f] otherwise.
}

@defproc[(constructor? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is an ECMAScript function which
  can be used in a @racketfont{new} expression, @racket[#f] otherwise.
}
