#lang racket/base

(require racket/class
         racket/lazy-require
         racket/math
         "../private/object.rkt"
         (only-in "object.rkt" Object%)
         "util.rkt")

(lazy-require
 ["../convert.rkt" (to-number)])

(provide get-properties)

(define (get-properties)
  `(["Math" . ,math]))

(define math (new Object%))

(define-syntax-rule (wrap-math fn arg ...)
  (native-method (arg ...)
    (fn (to-number arg) ...)))

(define-object-properties math
  ["E" (exp 1)]
  ["LN10" (log 10)]
  ["LN2" (log 2)]
  ["LOG2E" (/ (log 2))]
  ["LOG10E" (/ (log 10))]
  ["PI" pi]
  ["SQRT1_2" (sqrt 1/2)]
  ["SQRT2" (sqrt 2)]
  ["abs" (wrap-math abs x)]
  ["acos" (wrap-math acos x)]
  ["asin" (wrap-math asin x)]
  ["atan" (wrap-math atan x)]
  ["atan2" (wrap-math atan y x)]
  ["ceil" (wrap-math ceiling x)]
  ["cos" (wrap-math cos x)]
  ["exp" (wrap-math exp x)]
  ["floor" (wrap-math floor x)]
  ["log" (wrap-math log x)]
  ["max" (wrap-math max v1 v2)]
  ["min" (wrap-math min v1 v2)]
  ["pow" (wrap-math expt x y)]
  ["random" (wrap-math random)]
  ["round" (wrap-math round x)]
  ["sin" (wrap-math sin x)]
  ["sqrt" (wrap-math sqrt x)]
  ["tan" (wrap-math tan x)])
