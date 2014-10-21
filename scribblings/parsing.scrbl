#lang scribble/manual
@(require (for-label racket/base
                     racket/contract
                     "../ast.rkt"
                     "../parse.rkt"))

@title{Parsing ECMAScript}

@defmodule[ecmascript/parse]

@defproc[(read-program
          [source-name any/c (object-name in)]
          [in input-port? (current-input-port)])
         (or/c (listof source-element?) eof-object?)]{
}
