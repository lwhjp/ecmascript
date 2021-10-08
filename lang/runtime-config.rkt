#lang racket/base

(provide configure)

(require ecmascript/private/eval)

(define (configure data)
  (current-read-interaction eval-read-interaction))
