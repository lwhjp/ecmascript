#lang s-exp syntax/module-reader
ecmascript
#:read ecma:read
#:read-syntax ecma:read-syntax
#:language-info #(ecmascript/lang/language-info get-language-info #f)
#:whole-body-readers? #t

(require "read.rkt")
