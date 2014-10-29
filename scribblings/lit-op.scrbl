#lang scribble/manual
@(require (for-label "../private/helpers.rkt"
                     "../private/operator.rkt"
                     "../types.rkt"))

@title{Literals and Operators}

@section{Literals}

@declare-exporting[ecmascript/private/helpers]

@defproc[(array [v value?] ...) object?]{
}

@defform[(object [property-id property-definition] ...)
         #:grammar
         [(property-definition (code:line expr)
                               (code:line #:get get-fn)
                               (code:line #:set set-fn))]]{
}

@defproc[(regexp [pattern string?] [flags string?]) object?]{
}

@section{Operators}

@declare-exporting[ecmascript/private/operator]
