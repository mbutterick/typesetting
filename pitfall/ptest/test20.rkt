#lang racket/base
(require pitfall/pdftest)

;; subset OTF font
(define-runtime-path otf-path "assets/charter.otf")

;; embed otf

(define (proc doc)
  ;; Register a font name for use later
  (send doc register-font "the-font" (path->string otf-path))

  ;; Set the font, draw some text
  (send* doc
    [font "the-font"]
    [font-size 40]
    [text "Embedded OTF" 100 100 (hash 'width #f)]))

;; test against non-subsetted font version
(define-runtime-path this "test20rkt.pdf")
(make-doc this #f proc #:test #f)

#;(define-runtime-path that "test20crkt.pdf")
#;(make-doc that #t proc #:pdfkit #f)
