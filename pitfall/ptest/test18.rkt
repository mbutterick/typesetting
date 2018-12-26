#lang racket/base
(require pitfall/pdftest)

;; multiple lines

(define-runtime-path ttf-path "assets/fira.ttf")

(define (proc doc)
  ;; Register a font name for use later
  (register-font doc "the-font" (path->string ttf-path))

  ;; Set the font, draw some text
  [font doc "the-font"]
  [font-size doc 25]
  [text doc "In Xanadu did Kubla Khan" 100 100 (hash 'width #f)]
  [text doc "A stately pleasure dome decree:" 100 140 (hash 'width #f)]
  [text doc "Where Alph, the sacred river, ran" 100 180 (hash 'width #f)]
  [text doc "Through caverns measureless to man" 100 220 (hash 'width #f)]
  [text doc "Down to a sunless sea." 100 260 (hash 'width #f)])


(define-runtime-path this "test18rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test18crkt.pdf")
(make-doc that #t proc)