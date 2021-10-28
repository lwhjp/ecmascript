#lang racket/base

(require (only-in racket/list range)
         peg)

(provide (except-out (all-defined-out)
                     define-chars
                     define-char-class))

(define-syntax-rule (define-chars [NAME CHAR-EXP] ...)
  (begin
    (define-peg NAME (char CHAR-EXP)) ...))

(define-syntax-rule (define-char-class NAME (CHAR ...))
  (define-peg NAME (or (char CHAR) ...)))

(define-chars
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

(define-char-class USP ; Unicode Space_Separator (Zs)
  (#\u0020 #\u00A0 #\u1680 #\u2000 #\u2001 #\u2002
   #\u2003 #\u2004 #\u2005 #\u2006 #\u2007 #\u2008
   #\u2009 #\u200A #\u202F #\u205F #\u3000))

 ; FIXME: these are not correct
(define-peg UnicodeIDStart (or (range #\a #\z) (range #\A #\Z)))
(define-peg UnicodeIDContinue (or UnicodeIDStart (range #\0 #\9) (char #\_)))
