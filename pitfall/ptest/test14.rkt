#lang racket/base
(require pitfall/pdftest)

;; subset font with GPOS table
(define-runtime-path ttf-path "assets/fira.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (send doc registerFont "the-font" (path->string ttf-path))

  ;; Set the font, draw some text
  (send* doc
    [font "the-font"]
    [fontSize 25]
    [text "Hola Hola" 100 100 (hash 'width #f)]))

;; test against non-subsetted font version
(define-runtime-path this "test14rkt.pdf")
(make-doc this #f proc )

(define-runtime-path that "test14crkt.pdf")
(make-doc that #t proc #:pdfkit #f)
