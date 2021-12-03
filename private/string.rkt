#lang typed/racket/base

(require racket/port)

(provide (all-defined-out))

(struct es-string
  ([bytes : Bytes])
  #:transparent
  #:type-name ESString)

(: string->es-string (-> String ESString))
(: es-string->string (-> ESString String))
(: codepoints->es-string (-> (Listof Integer) ESString))
(: es-string->codepoints (-> ESString (Listof Integer)))
(: es-string=? (-> ESString ESString Boolean))
(: es-string<? (-> ESString ESString Boolean))
(: es-string-append (-> ESString * ESString))

(define es-empty-string (es-string #""))

(define (string->es-string str)
  (codepoints->es-string
   (map char->integer (string->list str))))

(define (es-string->string str)
  (list->string
   (map integer->char (es-string->codepoints str))))

(define (codepoints->es-string cps)
  (es-string
   (call-with-output-bytes
    (λ ([out : Output-Port])
      (define (write-cu [x : Integer])
        (write-byte (bitwise-and x #xFF) out)
        (write-byte (arithmetic-shift x -8) out))
      (let loop ([cps : (Listof Integer) cps])
        (unless (null? cps)
          (define cp (car cps))
          (cond
            [(<= cp #xFFFF) (write-cu cp)]
            [(> cp #x10FFFF) (error 'codepoints->es-string "invalid codepoint: ~a" cp)]
            [else
             (write-cu (+ #xD800 (arithmetic-shift (- cp #x10000) -11)))
             (write-cu (+ #xDC00 (bitwise-and (- cp #x10000) #x3FF)))])
          (loop (cdr cps))))))))

(define (es-string->codepoints str)
  (call-with-input-bytes
   (es-string-bytes str)
   (λ ([in : Input-Port])
     (define (read-cu)
       (define lo (read-byte in))
       (if (eof-object? lo)
           #f
           (bitwise-ior
            lo
            (arithmetic-shift (cast (read-byte in) Byte) 8))))
     (let loop : (Listof Integer) ([lead : (Option Integer) #f])
       (define cu (read-cu))
       (cond
         [(not cu)
          (if lead (list lead) '())]
         [(<= #xDC00 cu #xDFFF)
          (if lead
              (cons lead (loop cu))
              (loop lead))]
         [(<= #xD800 cu #xDBFF)
          (cons (if lead
                    (+ #x10000
                       (bitwise-ior
                        (- lead #xDC00)
                        (arithmetic-shift (- cu #xD800) 11)))
                    cu)
                (loop #f))]
         [else
          (if lead
              (list* lead cu (loop #f))
              (cons cu (loop #f)))])))))

(define (es-string=? s1 s2)
  (bytes=? (es-string-bytes s1) (es-string-bytes s2)))

(define (es-string<? s1 s2)
  (bytes<? (es-string-bytes s1) (es-string-bytes s2)))

(define (es-string-append . strs)
  (es-string (apply bytes-append (map es-string-bytes strs))))
