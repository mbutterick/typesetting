#lang racket/base
(require pitfall/pdftest)

(define (proc doc)
  [fill-color doc "blue"]
  [font doc "Helvetica" 30]
  [translate doc 50 50]
  [text doc "Here is a link!" 100 100 (hash
                                       'link "http://google.com/"
                                       'underline #t
                                       'width #f)])

(define-runtime-path this "test11rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test11crkt.pdf")
(make-doc that #t proc)
