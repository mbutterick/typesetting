#lang racket/base
(require pitfall/pdftest)

;; test ligatures (multiple ligs in a line)

(define-runtime-path ttf-path "assets/fira.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (send doc registerFont "the-font" (path->string ttf-path))

  ;; Set the font, draw some text
  (send* doc
    [font "the-font"]
    [fontSize 50]
    [text "The fifth rifle" 100 100 (hash 'width #f)]))


(define-runtime-path this "test17rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test17crkt.pdf")
(make-doc that #t proc)