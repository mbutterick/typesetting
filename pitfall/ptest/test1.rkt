#lang racket/base
(require pitfall/pdftest)

(define (proc doc)
  ;; Draw a triangle and a circle
  [save doc]
  [move-to doc 100 150]
  [line-to doc 100 250]
  [line-to doc 200 250]
  [fill doc "#FF3300"]

  [circle doc 280 200 50]
  [fill doc "#6600FF"])

(define-runtime-path this "test1rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test1crkt.pdf")
(make-doc that #t proc)

