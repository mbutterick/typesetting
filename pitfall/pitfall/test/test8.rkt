#lang pitfall/pdftest

(define-runtime-path pic "assets/test.png")

(define (proc doc)
  (send* doc
  [font "Helvetica-Bold"]
  [fontSize 25]
  [text "Another fantastic pic" 100 100 (hash 'lineBreak #f)]
  [image pic 100 160 (hash 'width 412)]))

; node's zlib.deflate makes smaller files, for some reason
; so don't compare file sizes in this case
(define-runtime-path this "test8rkt.pdf")
(make-doc this #f proc #:pdfkit #f) 

(define-runtime-path that "test8crkt.pdf")
(make-doc that #t proc #:pdfkit #f)
