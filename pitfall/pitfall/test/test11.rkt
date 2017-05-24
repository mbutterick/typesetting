#lang pitfall/pdftest

(define (proc doc)
  (send doc translate 200 300)
  (send* doc [path "M 0 0 v 100 h 100 v -100 h -100"]
    [stroke])

  (send doc translate 0 150)
  (send* doc [path "M 0 0 l 0 100 l 100 0 l 0 -100 l -100 0"]
    [stroke]))



(define-runtime-path that "test11crkt.pdf")
(make-doc that #t proc #:test #f)
