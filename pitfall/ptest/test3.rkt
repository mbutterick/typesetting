#lang racket/base
(require pitfall/pdftest)

(define (proc doc)
  (send doc text "Hello world"))

(define-runtime-path this "test3rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test3crkt.pdf")
(make-doc that #t proc)

