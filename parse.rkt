#lang racket/base

(require racket/contract/base
         racket/port
         peg/peg
         "ast.rkt"
         "lang/grammar.rkt"
         "lang/parse-helpers.rkt")

(provide/contract
 [read-program
  (->* ()
       (any/c input-port?)
       (listof (or/c declaration? statement?)))]
 [read-script
  (->* ()
       (any/c input-port?)
       (listof (or/c declaration? statement?)))])

(define (read-script [source-name (object-name (current-input-port))]
                     [in (current-input-port)])
  (parameterize ([current-source-name source-name])
    (peg (and Script (! (any-char))) (port->string in))))

(define read-program read-script)
