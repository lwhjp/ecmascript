#lang scribble/manual
@(require (for-label "../private/statement.rkt"
                     "../types.rkt"))

@title{Statements}

@declare-exporting[ecmascript/private/statement]

@defform[(block stmt ...)]{
  Evaluates each @racket[stmt] in order, returning
  the value of the last statement.
}

@defform[(var [id expr] ...)]{
  Updates the value of the variable bound to @racket[id]
  with the result of evaluating @racket[expr].
}

@defform[(empty-statement)]{
  Does nothing.
}

@defform*[((if test true)
           (if test true false))]{
  Evaluates @racket[test], then executes @racket[true] if
  the result of applying @racket[to-boolean] to the resulting
  value is @racket[#t]. Otherwise, @racket[false] is executed
  if supplied.
}

@defform[(for maybe-init maybe-test maybe-update body ...)
         #:grammar
         [(maybe-init (code:line)
                      (code:line #:init init))
          (maybe-test (code:line)
                      (code:line #:test test))
          (maybe-update (code:line)
                        (code:line #:update update))]]{
}

@defform[(while test body ...)]{
  Repeatedly executes @racket[body] so long as the result of
  @racket[(to-boolean test)] is @racket[#t] beforehand.

  Equivalent to @racketblock[(for #:test test body ...)]
}

@defform*[((break)
           (break label))]{
  Aborts the enclosing loop, or the loop with label
  @racket[label]. This form is illegal outside of a
  @racketfont{for} or @racketfont{while} loop.
}

@defform*[((continue)
           (continue label))]{
  Jumps to the next iteration of the enclosing loop,
  or the loop with label @racket[label]. This form is
  illegal outside of a @racketfont{for} or
  @racketfont{while} loop.
}

@defform[(with expr form ...)]{
}

@defform[(label id stmt)]{
}

@defform[(throw expr)]{
}

@defform[(try form ... maybe-catch maybe-finally)
         #:grammar
         [(maybe-catch (code:line)
                       (code:line #:catch catch-id catch-form ...))
          (maybe-finally (code:line)
                         (code:line #:finally finally-form ...))]]{
}
