#lang pitfall/pdftest

(define (proc doc)
  (send* doc
    [moveTo 0 20]
    [lineTo 100 160]
    [quadraticCurveTo 130 200 150 120]
    [bezierCurveTo 190 -40 200 200 300 150]  
    [lineTo 400 90]                         
    [stroke])

  (send* doc [translate 0 200])

  (send* doc
    [path "M 0,20 L 100,160 Q 130,200 150,120 C 190,-40 200,200 300,150 L 400,90"]                        
    [stroke]))

(define-runtime-path that "test09crkt.pdf")
(make-doc that #t proc)
