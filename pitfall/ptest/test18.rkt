#lang racket/base
(require pitfall/pdftest)

;; multiple lines

(define-runtime-path ttf-path "assets/fira.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (send doc registerFont "the-font" (path->string ttf-path))

  ;; Set the font, draw some text
  (send* doc
    [font "the-font"]
    [fontSize 25]
    [text "In Xanadu did Kubla Khan" 100 100 (hash 'width #f)]
    [text "A stately pleasure dome decree:" 100 140 (hash 'width #f)]
    [text "Where Alph, the sacred river, ran" 100 180 (hash 'width #f)]
    [text "Through caverns measureless to man" 100 220 (hash 'width #f)]
    [text "Down to a sunless sea." 100 260 (hash 'width #f)]))


(define-runtime-path this "test18rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test18crkt.pdf")
(make-doc that #t proc #:pdfkit #f)