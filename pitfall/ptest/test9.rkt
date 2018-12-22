#lang racket/base
(require pitfall/pdftest)

(define (proc doc)
  (send* doc
    [move-to 0 20]
    [line-to 100 160]
    [quadratic-curve-to 130 200 150 120]
    [bezier-curve-to 190 -40 200 200 300 150]  
    [line-to 400 90]                         
    [stroke])

  (send* doc [translate 0 200])

  (send* doc
    [path "M 0,20 L 100,160 Q 130,200 150,120 C 190,-40 200,200 300,150 L 400,90"]                        
    [stroke]))

(define-runtime-path this "test9rkt.pdf")
(make-doc this #false proc)

(define-runtime-path that "test9crkt.pdf")
(make-doc that #t proc)
