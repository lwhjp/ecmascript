#lang racket/base

(require "private/eval.rkt"
         "private/init.rkt"
         "private/realm.rkt"
         "lang/main.rkt")

(provide (all-from-out "private/eval.rkt"
                       "lang/main.rkt"))

(current-realm (make-realm))
