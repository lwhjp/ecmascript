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
  (parameterize ([current-source-name source-name]
                 [current-output-port (open-output-nowhere)])
    (with-handlers ([exn:fail? peg-error->read-error])
      (peg (and Script (! (any-char))) in))))

(define read-program read-script)

(define (peg-error->read-error e)
  (define msg (exn-message e))
  (cond
    [(regexp-match? #rx"^peg: parse failed" msg)
     (let ([m (regexp-match #px"location \\(line (\\d+) column (\\d+)\\)" msg)])
       (raise (exn:fail:read
               msg
               (exn-continuation-marks e)
               (list (srcloc (current-source-name)
                             (string->number (cadr m))
                             (string->number (caddr m))
                             #f
                             1)))))]
    [else (raise e)]))
