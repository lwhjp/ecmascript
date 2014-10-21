#lang s-exp syntax/module-reader
ecmascript
#:read ecma:read
#:read-syntax ecma:read-syntax
#:whole-body-readers? #t

(require "../init.rkt"
         "../private/read.rkt")
