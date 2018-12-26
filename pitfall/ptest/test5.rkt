#lang racket/base
(require pitfall/pdftest)

(define-runtime-path death "assets/death.png")

(define (proc doc)
  [font doc "Times-Italic"]
  [font-size doc 25]
  [text doc "Some fantastic text!" 100 100 (hash 'lineBreak #f)]
  [image doc death 100 160 (hash 'width 412)])

(define-runtime-path this "test5rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test5crkt.pdf")
(make-doc that 'compress proc)
