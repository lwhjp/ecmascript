#lang racket/base

(require parser-tools/lex
         ragg/support
         (prefix-in : parser-tools/lex-sre))

(provide lex)

(define-lex-abbrev white-space
  (:or #\u0009 #\u000B #\u000C #\u0020 #\u00A0 #\uFEFF))

(define-lex-abbrev line-terminator
  (:or #\u000A #\u000D #\u2028 #\u2029))

(define-lex-abbrev comment
  (:or (:: "/*" (complement (:: any-string "*/" any-string)) "*/")
       (:: "//" (:* (char-complement line-terminator)))))

(define-lex-abbrevs
  [identifier (:: identifier-start (:* identifier-part))]
  [identifier-start (:or alphabetic #\$ #\_)]
  [identifier-part (:or identifier-start numeric)])

(define-lex-abbrevs
  [keyword
   (:or "break" "case" "catch" "continue" "debugger" "default" "delete"
        "do" "else" "finally" "for" "function" "if" "in"
        "instanceof" "new" "return" "switch" "this" "throw" "try"
        "typeof" "var" "void" "while" "with")]
  [future-reserved-word
   (:or "class" "const" "enum" "export" "extends" "import" "super")]
  [future-reserved-word-strict
   (:or "implements" "interface" "let" "package" "private" "protected"
        "public" "static" "yield")])

;; 7.6 Punctuators
(define-lex-abbrevs
  [punctuator
   (:or "{"   "}"    "("   ")"   "["   "]"
        "."   ";"    ","   "<"   ">"   "<="
        ">="  "=="   "!="  "===" "!=="
        "+"   "-"    "*"   "%"   "++"  "--"
        "<<"  ">>"   ">>>" "&"   "|"   "^"
        "!"   "~"    "&&"  "||"  "?"   ":"
        "="   "+="   "-="  "*="  "%="  "<<="
        ">>=" ">>>=" "&="  "|="  "^=")]
  [div-punctuator
   (:or "/"   "/=")])

(define-lex-abbrevs
  [null-literal "null"]
  [boolean-literal (:or "true" "false")]
  [numeric-literal (:or decimal-literal hex-integer-literal)]
  [decimal-literal
   (:: (:or (:: decimal-integer-literal #\. (:* decimal-digit))
            (:: #\. (:+ decimal-digit))
            decimal-integer-literal)
       (:? (:: (:or #\e #\E)
               (:or #\+ #\- nothing)
               (:+ decimal-digit))))]
  [decimal-integer-literal (:or #\0 (:: (:- decimal-digit #\0) (:* decimal-digit)))]
  [decimal-digit (:/ #\0 #\9)]
  [hex-integer-literal (:: (:or "0x" "0X") (:+ (:/ #\0 #\9 #\a #\f #\A #\F)))]
  [string-literal
   (:or (:: #\" (:* (:or (:: #\\ any-char) (char-complement #\"))) #\")
        (:: #\' (:* (:or (:: #\\ any-char) (char-complement #\'))) #\'))]
  [regexp-literal
   (:: #\/ (:+ (:or (:: #\\ any-char) (char-complement #\/))) #\/ (:* identifier-part))])

(define (parse-number s)
  (cond
    [(regexp-match? #rx"^0[xX]" s)
     (string->number (substring s 2) 16)]
    [else (string->number s)]))

(define (parse-string s)
  (define (parse-escape esc)
    (define c (string-ref esc 1))
    (case c
      [(#\u #\x)
       (string
        (integer->char
         (string->number
          (substring esc 2)
          16)))]
      [(#\u000A #\u000D #\u2028 #\u2029) ""]
      [else
       (hash-ref
        #hasheqv((#\b . "\u0008")
                 (#\t . "\u0009")
                 (#\n . "\u000A")
                 (#\v . "\u000B")
                 (#\f . "\u000C")
                 (#\r . "\u000D"))
        c
        (string c))]))
  (define end (sub1 (string-length s)))
  (define escapes
    (regexp-match-positions*
     #px"\\\\([^ux]|x[[:xdigit:]]{2}|u[[:xdigit:]]{4})"
     s 1 end))
  (let loop ([begin 1]
             [escapes escapes])
    (if (null? escapes)
        (substring s begin end)
        (string-append
         (substring s begin (caar escapes))
         (parse-escape (substring s (caar escapes) (cdar escapes)))
         (loop (cdar escapes) (cdr escapes))))))

(define (parse-regexp s)
  (define m
    (regexp-match #rx"^\\/(.+?)\\/([^\\/]*)$" s))
  (list (cadr m) (caddr m)))

(define lex
  (lexer-src-pos
   [(:+ white-space) 'WS]
   [line-terminator 'EOL]
   [comment 'COMMENT]
   [(:or keyword future-reserved-word punctuator
         div-punctuator null-literal boolean-literal)
    lexeme]
   [identifier (token 'IDENTIFIER (string->symbol lexeme))]
   [numeric-literal (token 'NUMERIC (parse-number lexeme))]
   [string-literal (token 'STRING (parse-string lexeme))]
   [regexp-literal (token 'REGEXP (parse-regexp lexeme))]
   [(eof) (return-without-pos 'EOF)]))
