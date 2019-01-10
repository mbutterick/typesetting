#lang racket/base
(require pitfall/pdftest)

;; subset font with GPOS table
(define-runtime-path ttf-path "assets/fira.ttf")

; test ss03 (alternate ampersand form)

(define (proc doc)
  ;; Register a font name for use later
  (register-font doc "the-font" (path->string ttf-path))

  ;; Set the font, draw some text
  [font doc "the-font"]
  [font-size doc 100]
  [text doc "A&B" 100 100]
  [text doc "X&Y" 100 200 #:features (list (cons #"ss03" 1))])

;; test against non-subsetted font version
(define-runtime-path this "test19rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test19crkt.pdf")
(make-doc that #t proc)
