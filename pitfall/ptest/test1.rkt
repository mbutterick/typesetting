#lang racket/base
(require pitfall/pdftest)

(define (proc doc)
  ;; Draw a triangle and a circle
  (send* doc
    [save]
    [moveTo 100 150]
    [lineTo 100 250]
    [lineTo 200 250]
    [fill "#FF3300"])

  (send* doc
    [circle 280 200 50]
    [fill "#6600FF"]))

(define-runtime-path this "test1rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test1crkt.pdf")
(make-doc that #t proc)

