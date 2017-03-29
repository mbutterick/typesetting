#lang br
(require brag/support)
(provide make-tokenizer)

(define-lex-abbrev digit (char-set "0123456789"))
(define-lex-abbrev hex-digit (:or digit (char-set "ABCDEF")))
(define-lex-abbrev digits (:+ digit))
(define-lex-abbrev sign (:? (:or "+" "-")))
(define-lex-abbrev blackspace (:~ whitespace))

(define-lex-abbrev nonreg-char (:seq "#" hex-digit hex-digit))

(define (make-tokenizer src port)
  (port-count-lines! port)
  (lexer-file-path src)
  (define lex-once
    (lexer-srcloc
     [(eof) eof]
     [whitespace
      (token 'WHITESPACE lexeme #:skip? #t)]
     [(:or "true" "false") (token 'BOOLEAN (equal? lexeme "true"))]
     [(:seq sign digits) (token 'INT (string->number lexeme))]
     [(:seq sign (:or (:seq digits "." (:? digits))
                      (:seq "." digits)))
      (token 'REAL (string->number lexeme))]
     [(:seq "/" (:+ (:or nonreg-char blackspace)))
      (token 'NAME lexeme)]
      ["null" (token 'NULL 'null)]
      [any-char (token 'CHAR lexeme)]))
(λ () (lex-once port)))