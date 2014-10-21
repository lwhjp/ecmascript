#lang scribble/manual
@(require (for-label "../main.rkt"
                     (only-in racket
                              prefix-in)))

@title{ECMAScript as S-expressions}

@defmodule[ecmascript]

This module contains definitions which conflict with the
@racketmodfont{racket} library, so you should require it using
@racket[prefix-in] or start your module with
@racketmod[s-exp ecmascript]
