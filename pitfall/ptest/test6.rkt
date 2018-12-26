#lang racket/base
(require pitfall/pdftest)

(define (proc doc)
  [text doc "Page 1"]
  [add-page doc]
  [text doc "Page 2"]
  [add-page doc]
  [text doc "Page 3"]
  [add-page doc]
  [text doc "Page 4"]
  [add-page doc]
  [text doc "Page 5"]
  [add-page doc]
  [text doc "Page 6"])

(define-runtime-path this "test6rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test6crkt.pdf")
(make-doc that 'compress proc)

