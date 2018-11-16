#lang racket/base
(require pitfall/pdftest)

(define (proc doc)
  (send* doc
    [text "Page 1"]
    [addPage]
    [text "Page 2"]
    [addPage]
    [text "Page 3"]
    [addPage]
    [text "Page 4"]
    [addPage]
    [text "Page 5"]
    [addPage]
    [text "Page 6"]))

(define-runtime-path this "test6rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test6crkt.pdf")
(make-doc that #t proc)

