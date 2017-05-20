#lang pitfall/pdftest

(define-runtime-path death "death.png")

(define (proc doc)
  (send* doc
  [font "Times-Italic"]
  [fontSize 25]
  [text "Some fantastic text!" 100 100 (hash 'lineBreak #f)]
  [image death 100 160 (hash 'width 412)]))

(define-runtime-path this "test5rkt.pdf")
(make-doc this #f proc #:test #t)

(define-runtime-path that "test5crkt.pdf")
(make-doc that #t proc)
