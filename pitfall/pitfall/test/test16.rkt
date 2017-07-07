#lang pitfall/pdftest

;; test ligatures (GSUB parsing + substitution)

(define-runtime-path ttf-path "assets/fira.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (send doc registerFont "the-font" (path->string ttf-path))

  ;; Set the font, draw some text
  (send* doc
    [font "the-font"]
    [fontSize 100]
    [text "fi" 100 100 (hash 'width #f)]))


(define-runtime-path this "test16rkt.pdf")
(make-doc this #f proc #:test #f)

#;(define-runtime-path that "test16crkt.pdf")
#;(make-doc that #t proc #:pdfkit #f)