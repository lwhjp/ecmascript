#lang racket/base

(provide configure)

(require ecmascript/eval)

(define (configure data)
  (current-read-interaction eval-read-interaction))
