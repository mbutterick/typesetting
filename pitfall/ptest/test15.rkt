#lang racket/base
(require pitfall/pdftest)

;; test kerning from GPOS

(define-runtime-path ttf-path "assets/fira.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (register-font doc "the-font" (path->string ttf-path))

  ;; Set the font, draw some text
  [font doc "the-font"]
  [font-size doc 25]
  [text doc "HTAVATH" 100 100])


(define-runtime-path this "test15rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test15crkt.pdf")
(make-doc that #t proc)