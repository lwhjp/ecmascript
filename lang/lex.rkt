#lang racket/base

(require parser-tools/lex
         ragg/support
         (prefix-in : parser-tools/lex-sre))

(provide lex)

(define-lex-abbrevs
  [TAB    #\u0009]  ; CHARACTER TABULATION
  [LF     #\u000A]  ; LINE FEED
  [VT     #\u000B]  ; LINE TABULATION
  [FF     #\u000C]  ; FORM FEED
  [CR     #\u000D]  ; CARRIAGE RETURN
  [SP     #\u0020]  ; SPACE
  [NBSP   #\u00A0]  ; NO-BREAK SPACE
  [ZWNJ   #\u200C]  ; ZERO WIDTH NON-JOINER
  [ZWJ    #\u200D]  ; ZERO WIDTH JOINER
  [LS     #\u2028]  ; LINE SEPARATOR
  [PS     #\u2029]  ; PARAGRAPH SEPARATOR
  [ZWNBSP #\uFEFF]) ; ZERO WIDTH NO-BREAK SPACE

(define-lex-abbrev USP ; Unicode Space_Separator (Zs)
  (:or #\u0020 #\u00A0 #\u1680 #\u2000 #\u2001 #\u2002
       #\u2003 #\u2004 #\u2005 #\u2006 #\u2007 #\u2008
       #\u2009 #\u200A #\u202F #\u205F #\u3000))

(define-lex-abbrevs ; FIXME: these are not correct
  [unicode-id-start alphabetic]
  [unicode-id-continue (:or unicode-id-start numeric #\_)])

(define-lex-abbrevs
  [white-space (:or TAB VT FF SP NBSP ZWNBSP USP)]
  [line-terminator (:or LF CR LS PS)]
  [line-terminator-sequence (:or LF CR LS PS (:: CR LF))])

(define-lex-abbrev comment
  (:or (:: "/*" (complement (:: any-string "*/" any-string)) "*/")
       (:: "//" (:* (char-complement line-terminator)))))

(define-lex-abbrevs
  [identifier (:: (:? #\#) identifier-start (:* identifier-part))]
  [identifier-start (:or unicode-id-start #\$ #\_)]
  [identifier-part (:or unicode-id-continue #\$ ZWNJ ZWJ)])

;; FIXME: await, yield etc can be identifiers in some cases, so
;; the parser needs to be able to handle both interpretations
(define-lex-abbrev reserved-word
  (:or "await" "break"
       "case" "catch" "class" "const" "continue"
       "debugger" "default" "delete" "do"
       "else" "enum" "export" "extends"
       "false" "finally" "for" "function"
       "if" "import" "in" "instanceof"
       "new" "null" "return" "super" "switch"
       "this" "throw" "true" "try" "typeof"
       "var" "void" "while" "with" "yield"))

(define-lex-abbrevs
  [punctuator
   (:or "{"   "}"    "("   ")"   "["   "]"
        "."   "..."  ";"   ","   "<"   ">"
        "<="  ">="   "=="  "!="  "===" "!=="
        "+"   "-"    "*"   "%"   "++"  "--"
        "<<"  ">>"   ">>>" "&"   "|"   "^"
        "!"   "~"    "&&"  "||"  "?"   ":"
        "="   "+="   "-="  "*="  "%="  "**="
        "<<=" ">>=" ">>>=" "&="  "|="  "^="
        "&&=" "||="  "??=" "=>")]
  [div-punctuator
   (:or "/"   "/=")])

(define-lex-abbrevs
  [numeric-literal
   (:: (:or decimal-literal non-decimal-integer-literal) (:? #\n))]
  [decimal-literal
   (:: (:or (:: decimal-integer-literal #\. (:? decimal-integer-literal))
            (:: #\. decimal-integer-literal)
            decimal-integer-literal)
       (:? (:: (:or #\e #\E)
               (:or #\+ #\- nothing)
               decimal-integer-literal)))]
  [decimal-integer-literal (:+ decimal-digit)]
  [decimal-digit (:/ "09")]
  [non-decimal-integer-literal
   (:or (:: (:or "0b" "0B") (:+ (char-set "01")))
        (:: (:or "0o" "0O") (:+ (char-set "01234567")))
        (:: (:or "0x" "0X") (:+ hex-digit)))]
  [hex-digit (:/ "09AFaf")])

(define-lex-abbrevs
  [string-literal
   (:or (:: #\" (:* double-string-character) #\")
        (:: #\' (:* single-string-character) #\'))]
  [double-string-character
   (:or (:- any-char #\" #\\ line-terminator)
        LS
        PS
        (:: #\\ (:or line-terminator any-char)))]
  [single-string-character
   (:or (:- any-char #\' #\\ line-terminator)
        LS
        PS
        (:: #\\ (:or line-terminator any-char)))])

(define-lex-abbrevs
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
   [(:or reserved-word punctuator div-punctuator) lexeme]
   [identifier (token 'IDENTIFIER (string->symbol lexeme))]
   [numeric-literal (token 'NUMERIC (parse-number lexeme))]
   [string-literal (token 'STRING (parse-string lexeme))]
   ; TODO: disable regexp for now until we have a smarter parser
   ;[regexp-literal (token 'REGEXP (parse-regexp lexeme))]
   [(eof) (return-without-pos 'EOF)]))
