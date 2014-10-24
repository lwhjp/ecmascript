#lang scribble/manual
@(require (for-label "../main.rkt"
                     "../types.rkt"))

@title{Types}

@declare-exporting[ecmascript/types]

@section{Type Instances}

@defthing[undefined undefined?]{
  The undefined value.
}

@defthing[null null?]{
  The null value.
}

@section{Additional Type Utilities}

@defmodule[ecmascript/types]

@subsection{Type Predicates}

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

@subsection{Reference Type}

@defstruct[reference
           ([base (or/c object? (is-a?/c environment-record%) undefined?)]
            [name string?]
            [strict? boolean?])]{
  An ECMAScript reference.
}

@subsection{Type Conversions}

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
