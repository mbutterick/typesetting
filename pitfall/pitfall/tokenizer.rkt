#lang at-exp br
(require brag/support)
(provide make-tokenizer)

(define-lex-abbrev digit (char-set "0123456789"))
(define-lex-abbrev hex-digit (:or digit (char-set "ABCDEF")))
(define-lex-abbrev digits (:+ digit))
(define-lex-abbrev optional-sign (:? (:or "+" "-")))
(define-lex-abbrev pdf-whitespace (char-set "\u0000\t\n\f\r "))
(define-lex-abbrev pdf-delimiter (char-set "()<>[]{}/%"))
#;(define-lex-abbrev pdf-reg)
(define-lex-abbrev blackspace (:~ pdf-whitespace))
(define-lex-abbrev not-right-paren (:~ ")"))
(define-lex-abbrev substring (:seq "(" (:* not-right-paren) ")"))

(define-lex-abbrev nonreg-char (:seq "#" hex-digit hex-digit))

(define (make-tokenizer port [src #f])
  (port-count-lines! port)
  (lexer-file-path src)
  (define lex-one-token
    (lexer-srcloc
     [(eof) eof]
     [(:seq "%%EOF" any-string) eof]
     [(:seq digits (:+ pdf-whitespace) digits (:+ pdf-whitespace) "R")
      (token 'INDIRECT-OBJECT-REF-TOK (string-split lexeme))]
     [(:seq "%PDF-" digits "." digits) (token 'PDF-VERSION (string->number (trim-ends "%PDF-" lexeme "")))]
     [(:or pdf-whitespace
           (from/stop-before "%" #\newline)) (token 'IGNORE lexeme #:skip? #t)]
     [(:or "true" "false") (token 'BOOLEAN (equal? lexeme "true"))]
     [(:seq optional-sign digits) (token 'INT (string->number lexeme))]
     [(:seq optional-sign (:or (:seq digits "." (:? digits))
                               (:seq "." digits)))
      (token 'REAL (string->number lexeme))]
     [(from/stop-before "/" (:or pdf-delimiter pdf-whitespace)) (token 'NAME lexeme)]
     ["null" (token 'NULL 'null)]
     [(:seq "(" (:* (:or not-right-paren substring)) ")") (token 'STRING-TOK lexeme)]
     [(:seq hex-digit hex-digit) (token 'HEX-DIGIT-PAIR (string->number lexeme 16))]
     [(:or "<" ">" "[" "]" "obj" "endobj") (token lexeme lexeme)]
     [(from/to "stream" "endstream") (token 'STREAM-DATA (string-trim (trim-ends "stream" lexeme "endstream") "\n"))]
     [any-char (token 'CHAR lexeme)]))
  (λ () (lex-one-token port)))

(module+ test
  (apply-tokenizer-maker make-tokenizer @string-append{(s(t)r) << /A (B) >>}))