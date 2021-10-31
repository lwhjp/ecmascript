#lang racket

(require rackunit
         yaml
         ecmascript/eval)

(define test262-home
  (bytes->path
   (or (environment-variables-ref (current-environment-variables) #"TEST262_HOME")
       (error "TEST262_HOME not set"))))

(define (load-test-file-metadata path)
  (call-with-input-file path
    (λ (in)
      (cond
        [(regexp-match #rx"/\\*---(.*?)---\\*/" in)
         => (compose1 string->yaml bytes->string/utf-8 second)]
        [else (hash)]))))

(define (include/harness name [realm (current-realm)])
  (es-eval (build-path test262-home "harness" name) realm))

(define make-test-realm
  (let ([prototype
         (lazy
          (let ([realm (make-realm)])
            (for ([name (in-list '("assert.js" "sta.js"))])
              (include/harness name realm))
            realm))])
    (λ () (send (force prototype) clone))))

(define (run-test262-file path)
  (define base-name
    (let-values ([(base name must-be-dir?) (split-path path)])
      (path->string (path-replace-extension name ""))))
  (define meta (load-test-file-metadata path))
  (define flags (hash-ref meta "flags" '()))
  (define includes (hash-ref meta "includes" '()))
  (define negative (hash-ref meta "negative" #f))
  (define (do-test strict?)
    ; TODO: strict mode, CanBlock, locale
    (define name (if strict? (string-append base-name " (Strict Mode)") base-name))
    (define run
      (thunk
       (parameterize ([current-realm (make-test-realm)])
         (for-each include/harness includes)
         ; TODO: host-defined functions
         (es-eval path))))
    (if negative
        (test-exn
         name
         exn:fail? ; TODO: check phase, type
         run)
        (test-not-exn
         name
         run)))
  (cond
    [(member "module" flags) (void)] ; TODO
    [(member "raw" flags) (do-test #f)]
    [(member "async" flags) (void)] ; TODO
    [else
     (unless (member "onlyStrict" flags)
       (do-test #f))
     #;(unless (member "noStrict" flags)
       (do-test #t))]))

(define test262-all
  (let ([test-files (find-files (λ (path) (regexp-match? #rx"\\.js" path))
                                (build-path test262-home "test"))])
    (test-suite
     "Test262"
     (for ([path (in-list test-files)])
       (run-test262-file path)))))
