#lang racket/base

(require parser-tools/lex
         ragg/support
         (prefix-in : parser-tools/lex-sre))

(provide es-lex)

;; 7.1 White Space
(define-lex-abbrev white-space
  (:or #\u0009 #\u000B #\u000C #\u0020))

;; 7.2 Line Terminators
(define-lex-abbrev line-terminator
  (:or #\u000A #\u000D))

;; 7.3 Comments
(define-lex-abbrev comment
  (:or (:: "/*" (complement (:: any-string "*/" any-string)) "*/")
       (:: "//" (:* (char-complement line-terminator)))))

;; 7.4 Tokens
(define-lex-abbrevs
  [keyword
   (:or "break" "continue" "delete" "else"
        "for" "function" "if" "in"
        "new" "return" "this" "typeof"
        "var" "void" "while" "with")]
  [future-reserved-word
   (:or "case" "catch" "class" "const"
        "debugger" "default" "do" "enum"
        "export" "extends" "finally" "import"
        "super" "switch" "throw" "try")])

;; 7.5 Identifiers
(define-lex-abbrevs
  [identifier (:: identifier-letter (:* (:or identifier-letter decimal-digit)))]
  [identifier-letter (:or (:/ #\a #\z) #\$ #\_)])

;; 7.6 Punctuators
(define-lex-abbrev punctuator
  (:or #\= #\> #\< "==" "<=" ">="
       "!=" #\, #\! #\~ #\? #\:
       #\. "&&" "||" "++" "--" #\+
       #\- #\* #\/ #\& #\| #\^
       #\% "<<" ">>" ">>>" "<<="
       ">>=" ">>>=" #\( #\) #\{
       #\} #\[ #\] #\;))

;; 7.7 Literals
(define-lex-abbrevs
  [null-literal "null"]
  [boolean-literal (:or "true" "false")]
  [numeric-literal
   (:or (:: #\0 (:+ (:/ #\0 #\7)))
        (:: (:or "0x" "0X") (:+ (:/ #\0 #\9 #\a #\f #\A #\F)))
        (:: (:or (:: decimal-integer-literal #\. (:* decimal-digit))
                 (:: #\. (:+ decimal-digit))
                 decimal-integer-literal)
            (:? (:: (:or #\e #\E)
                    (:or #\+ #\- nothing)
                    (:+ decimal-digit)))))]
  [decimal-integer-literal (:or #\0 (:: (:- decimal-digit #\0) (:* decimal-digit)))]
  [decimal-digit (char-range #\0 #\9)]
  [string-literal #| FIXME |#
   (:or (:: #\" (:* (char-complement #\")) #\")
        (:: #\' (:* (char-complement #\')) #\'))])

(define parse-number string->number)

(define (parse-string s)
  (substring s 1 (sub1 (string-length s))))

(define es-lex
  (lexer-src-pos
   [(:or keyword future-reserved-word punctuator null-literal boolean-literal) lexeme]
   [identifier (token 'IDENTIFIER (string->symbol lexeme))]
   [numeric-literal (token 'NUMERIC (parse-number lexeme))]
   [string-literal (token 'STRING (parse-string lexeme))]
   [comment 'COMMENT]
   [line-terminator 'EOL]
   [(:+ white-space) 'WS]
   [(eof) (return-without-pos 'EOF)]))
