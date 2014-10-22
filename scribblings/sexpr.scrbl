#lang scribble/manual
@(require (for-label "../function.rkt"
                     "../main.rkt"
                     "../types.rkt"
                     (only-in racket
                              prefix-in)))

@title{ECMAScript as S-expressions}

@defmodule[ecmascript]

This module contains definitions which conflict with the
@racketmodfont{racket} library, so you should require it using
@racket[prefix-in] or start your module with
@racketmod[s-exp ecmascript]

@section{Types}

@defmodule[ecmascript/types]

@defthing[undefined undefined?]{
  The undefined value.
}

@defthing[null null?]{
  The null value.
}

@defproc[(defined? [v any/c]) boolean?]{
  Returns @racket[#f] if @racket[v] is the undefined value, @racket[#t] otherwise.
}

@defproc[(undefined? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is the undefined value, @racket[#f] otherwise.
}

@defproc[(null? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is the null value, @racket[#f] otherwise.
}

@defproc[(boolean? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a boolean value, @racket[#f] otherwise.
}

@defproc[(number? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a number, @racket[#f] otherwise.
}

@defproc[(string? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a string, @racket[#f] otherwise.
}

@defproc[(primitive-value? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is the undefined value, the null
  value, a boolean, a number or a string.
}

@defproc[(object? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is an ECMAScript object, @racket[#f] otherwise.
}

@defproc[(value? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is an ECMAScript value (if either
  @racket[primitive-value?] or @racket[object?] is true), @racket[#f] otherwise.
}

@defproc[(to-primitive [v value?] [preferred (or/c 'number 'string #f) #f]) primitive-value?]{
  Converts @racket[v] to a primitive value. If @racket[preferred] is
  @racket['number] or @racket['string], the conversion process may
  return a value of that type. If @racket[v] is already a primitive
  value, it is returned unchanged.
}

@defproc[(to-boolean [v value?]) boolean?]{
  Converts @racket[v] to a boolean value.
}

@defproc[(to-number [v value?]) number?]{
  Converts @racket[v] to a number (possibly @racket[+nan.0]).
}

@defproc[(to-integer [v value?]) number?]{
  Converts @racket[v] to a number and returns the result of truncating
  it to an integer.
}

@defproc[(to-int32 [v value?]) number?]{
  Converts @racket[v] to a signed 32-bit integer.
}

@defproc[(to-uint32 [v value?]) number?]{
  Converts @racket[v] to an unsigned 32-bit integer.
}

@defproc[(to-uint16 [v value?]) number?]{
  Converts @racket[v] to an unsigned 16-bit integer.
}

@defproc[(to-string [v value?]) string?]{
  Returns a string representation of @racket[v].
}

@defproc[(to-object [v (and/c value? (not/c undefined?) (not/c null?))]) object?]{
  If @racket[v] is an ECMAScript object, it is returned unchanged.
  Otherwise, a new @racketfont{Boolean}, @racketfont{Number} or
  @racketfont{String} object is created with its value set to
  @racket[v].
}

@section{Functions}

@defmodule[ecmascript/function]

@defform[(function (id ...) body ...)]{
  Produces an ECMAScript function. Each parameter @racket[id] is bound
  in the ECMAScript environment, but is not bound as a Racket variable.
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

@defproc[(function? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is an ECMAScript function,
  @racket[#f] otherwise.
}

@defproc[(constructor? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is an ECMAScript function which
  can be used in a @racketfont{new} expression, @racket[#f] otherwise.
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
