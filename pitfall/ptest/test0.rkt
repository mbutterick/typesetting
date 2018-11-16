#lang racket/base
(require pitfall/pdftest)

(define-runtime-path this "test0rkt.pdf")
(make-doc this #f)

(define-runtime-path that "test0crkt.pdf")
(make-doc that #t)
