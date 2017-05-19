#lang pitfall/pdftest

(define (proc doc)
  (send* doc
  [font "Times-Italic"]
  [fontSize 25]
  [text "Some text with an embedded font!" 100 100 (hash 'lineBreak #f)]
  [image "assets/test.png" 100 160 (hash 'width 412)]
  [image "assets/test.jpeg" 190 400 (hash 'height 300)]))

(define-runtime-path this "test5rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test5crkt.pdf")
(make-doc that #t proc)

