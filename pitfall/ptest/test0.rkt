#lang racket/base
(require pitfall/pdftest)

(define-runtime-path pdf "test0rkt.pdf")
(make-doc pdf #f)

(define-runtime-path pdfc "test0crkt.pdf")
(make-doc pdfc #t)
