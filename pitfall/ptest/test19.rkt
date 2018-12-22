#lang racket/base
(require pitfall/pdftest)

;; subset font with GPOS table
(define-runtime-path ttf-path "assets/fira.ttf")

; test ss03 (alternate ampersand form)

(define (proc doc)
  ;; Register a font name for use later
  (send doc register-font "the-font" (path->string ttf-path))

  ;; Set the font, draw some text
  (send* doc
    [font "the-font"]
    [font-size 100]
    [text "A&B" 100 100 (hash 'width #f)]
    [text "X&Y" 100 200 (hash 'width #f 'features '(ss03))]))

;; test against non-subsetted font version
(define-runtime-path this "test19rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test19crkt.pdf")
(make-doc that #t proc)
