#lang racket/base
(require pitfall/pdftest)

(define (proc doc)
  [move-to doc 0 20]
  [line-to doc 100 160]
  [quadratic-curve-to doc 130 200 150 120]
  [bezier-curve-to doc 190 -40 200 200 300 150]  
  [line-to doc 400 90]                         
  [stroke doc]

  [translate doc 0 200]

  [path doc "M 0,20 L 100,160 Q 130,200 150,120 C 190,-40 200,200 300,150 L 400,90"]                        
  [stroke doc])

(define-runtime-path this "test9rkt.pdf")
(make-doc this #false proc)

(define-runtime-path that "test9crkt.pdf")
(make-doc that #t proc)
