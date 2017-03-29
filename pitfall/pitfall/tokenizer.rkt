#lang br
(require brag/support)
(provide make-tokenizer)

(define-lex-abbrev digit (char-set "0123456789"))
(define-lex-abbrev hex-digit (:or digit (char-set "ABCDEF")))
(define-lex-abbrev digits (:+ digit))
(define-lex-abbrev sign (:? (:or "+" "-")))
(define-lex-abbrev blackspace (:~ whitespace))
(define-lex-abbrev ascii-char (char-set ascii))

(define-lex-abbrev nonreg-char (:seq "#" hex-digit hex-digit))

(define (make-tokenizer src port)
  (port-count-lines! port)
  (lexer-file-path src)
  (define lex-once
    (lexer-srcloc
     [(eof) eof]
     [(:or whitespace
       (from/stop-before "%" "\n"))
      (token 'IGNORE lexeme #:skip? #t)]
     [(:or "true" "false") (token 'BOOLEAN (equal? lexeme "true"))]
     [(:seq sign digits) (token 'INT (string->number lexeme))]
     [(:seq sign (:or (:seq digits "." (:? digits))
                      (:seq "." digits)))
      (token 'REAL (string->number lexeme))]
     [(:seq "/" (:+ (:or nonreg-char alphabetic "_" numeric)))
      (token 'NAME lexeme)]
      ["null" (token 'NULL 'null)]
      [(from/to "(" ")") (token 'PAREN-TOK lexeme)]
      [(:seq hex-digit hex-digit) (token 'HEX-DIGIT-PAIR (string->number lexeme 16))]
      ["<" (token 'LEFT-ANGLE)]
      [">" (token 'RIGHT-ANGLE)]
      ["<<" (token 'DOUBLE-LEFT-ANGLE)]
      [">>" (token 'DOUBLE-RIGHT-ANGLE)]
      ["[" (token 'LEFT-BRACKET)]
      ["]" (token 'RIGHT-BRACKET)]
      [any-char (token 'CHAR lexeme)]))
(λ () (lex-once port)))