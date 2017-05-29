#lang pitfall/pdftest

(define (proc doc)
  (send* doc
    [fillColor "blue"]
    [font "Helvetica" 30]
    [text "Here is a link!" 100 100 (hash
                                     'link "http://google.com/"
                                     'underline #t
                                     'width #f)]))

(define-runtime-path this "test11rkt.pdf")
(make-doc this #f proc #:pdfkit #f)

(define-runtime-path that "test11crkt.pdf")
(make-doc that #t proc)
