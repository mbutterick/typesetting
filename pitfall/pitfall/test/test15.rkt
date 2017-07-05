#lang pitfall/pdftest

;; test kerning from GPOS

(define-runtime-path ttf-path "assets/fira.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (send doc registerFont "the-font" (path->string ttf-path))

  ;; Set the font, draw some text
  (send* doc
    [font "the-font"]
    [fontSize 25]
    [text "ATAVATA" 100 100 (hash 'width #f)]))


(define-runtime-path this "test15rkt.pdf")
(make-doc this #f proc #:test #f)

#;(define-runtime-path that "test15crkt.pdf")
#;(make-doc that #t proc #:pdfkit #f)