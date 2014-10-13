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
   (:or (:: #\" (:* (char-complement #\")) #\")
        (:: #\' (:* (char-complement #\')) #\'))])

(define parse-number string->number)

(define (parse-string s)
  (read (open-input-string s)))

(define lex
  (lexer-src-pos
   [(:or keyword future-reserved-word punctuator
         div-punctuator null-literal boolean-literal)
    lexeme]
   [identifier (token 'IDENTIFIER (string->symbol lexeme))]
   [numeric-literal (token 'NUMERIC (parse-number lexeme))]
   [string-literal (token 'STRING (parse-string lexeme))]
   [comment 'COMMENT]
   [line-terminator 'EOL]
   [(:+ white-space) 'WS]
   [(eof) (return-without-pos 'EOF)]))
