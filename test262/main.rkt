#lang racket

(require racket/sandbox
         rackunit
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

(define test-timeout-seconds 30)

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
    (define (run)
      (with-handlers ([exn:fail:resource?
                       (λ (e)
                         (fail-check "timeout"))])
        (with-limits test-timeout-seconds #f
          (parameterize ([current-realm (make-test-realm)])
            (for-each include/harness includes)
            ; TODO: host-defined functions
            (es-eval path)))))
    (if negative
        (test-exn
         name
         exn? ; TODO: check phase, type
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

(define (directory->tests path)
  (filter-map
   (λ (e)
     (define e-path (build-path path e))
     (cond
       [(regexp-match #rx"^(.*?)\\.js$" e)
        => (λ (m)
             (test-suite (second m) (run-test262-file e-path)))]
       [(directory-exists? e-path)
        (make-test-suite
         (path->string e)
         (directory->tests e-path))]
       [else #f]))
   (directory-list path)))

(define test262-all
  (make-test-suite
   "Test262"
   (directory->tests (build-path test262-home "test"))))
