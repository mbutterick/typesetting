#lang racket/base
(require pitfall/pdftest)

;; subset font with GPOS table
(define-runtime-path ttf-path "assets/fira.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (register-font doc "the-font" (path->string ttf-path))

  ;; Set the font, draw some text
  [font doc "the-font"]
  [font-size doc 25]
  [text doc "Hola Hola" 100 100 (hash 'width #f)])

;; test against non-subsetted font version
(define-runtime-path this "test14rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test14crkt.pdf")
(make-doc that #t proc)
