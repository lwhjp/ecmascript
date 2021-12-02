#lang racket/base

(require (for-syntax racket/base)
         "../private/string.rkt")

(provide (all-defined-out))

(define-syntax (es-string-literal stx)
  (syntax-case stx ()
    [(_ lit)
     (string? (syntax-e #'lit))
     (let* ([str (syntax-e #'lit)]
            [end (string-length str)])
       (define cps
         (let loop ([pos 0])
           (cond
             [(= end pos) '()]
             [(regexp-match #px"^\\\\([0-3][0-7]{0,2}|[4-7][0-7]?)" str pos)
              => (λ (m) (cons (string->number (cadr m) 8)
                              (loop (+ pos (string-length (car m))))))]
             [(regexp-match #px"^\\\\x([0-9A-Fa-f]{2})" str pos)
              => (λ (m) (cons (string->number (cadr m) 16)
                              (loop (+ pos 4))))]
             [(regexp-match #px"^\\\\u([0-9A-Fa-f]{4})" str pos)
              => (λ (m) (cons (string->number (cadr m) 16)
                              (loop (+ pos 6))))]
             [(regexp-match #px"^\\\\u\\{([0-9]+)\\}" str pos)
              => (λ (m) (cons (string->number (cadr m))
                              (loop (+ pos (string-length (car m))))))]
             [(regexp-match #px"^\\\\(\r\n?|.)" str pos)
              => (λ (m) (append (let ([e (cadr m)])
                                  (case e
                                    [("b") '(#x0008)]
                                    [("t") '(#x0009)]
                                    [("n") '(#x000A)]
                                    [("v") '(#x000B)]
                                    [("f") '(#x000C)]
                                    [("r") '(#x000D)]
                                    [else (map char->integer (string->list (cadr m)))]))
                                (loop (+ pos (string-length (car m))))))]
             [else (cons (char->integer (string-ref str pos))
                         (loop (add1 pos)))])))
       #`(codepoints->es-string '#,cps))]))
