#lang racket/base
(require pitfall/pdftest)

(define-runtime-path test-jpeg "assets/test.jpeg")

(define (proc doc)
  [font doc "Times-Italic"]
  [font-size doc 25]
  [text doc "Here comes a JPEG!" 100 100]
  [image doc test-jpeg 100 160 (hash 'width 412)])

(define-runtime-path this "test7rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test7crkt.pdf")
(make-doc that #t proc)
