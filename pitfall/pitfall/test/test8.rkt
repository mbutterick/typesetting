#lang pitfall/pdftest

(define-runtime-path pic "assets/test.png")

(define (proc doc)
  (send* doc
  [font "Helvetica-Bold"]
  [fontSize 25]
  [text "Another fantastic pic" 100 100 (hash 'lineBreak #f)]
  [image pic 100 160 (hash 'width 412)]))

(define-runtime-path this "test8rkt.pdf")
(make-doc this #f proc #:test #f)

#;(define-runtime-path that "test8crkt.pdf")
#;(make-doc that #t proc)
