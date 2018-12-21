#lang racket/base
(require pitfall/pdftest)

(define (proc doc)
  (send* doc
    [text "Page 1"]
    [add-page]
    [text "Page 2"]
    [add-page]
    [text "Page 3"]
    [add-page]
    [text "Page 4"]
    [add-page]
    [text "Page 5"]
    [add-page]
    [text "Page 6"]))

(define-runtime-path this "test6rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test6crkt.pdf")
(make-doc that 'compress proc)

