#lang racket/base
(require pitfall/pdftest)

;; subset OTF font
(define-runtime-path otf-path "assets/fira.otf")

;; embed otf

(define (proc doc)
  ;; Register a font name for use later
  (register-font doc "the-font" (path->string otf-path))

  ;; Set the font, draw some text
  [font doc "the-font"]
  [font-size doc 40]
  [text doc "Fira OTF rifle fire" 100 100])

;; test against non-subsetted font version
(define-runtime-path this "test20rkt.pdf")
(make-doc this #f proc #:test #f)

#;(define-runtime-path that "test20crkt.pdf")
#;(make-doc that #t proc #:pdfkit #f)
