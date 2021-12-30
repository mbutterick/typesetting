#lang racket/base
(require pitfall/pdftest)

(define (proc doc)

  ;; curved path as bezier
  [move-to doc 0 20]
  [line-to doc 100 160]
  [quadratic-curve-to doc 130 200 150 120]
  [bezier-curve-to doc 190 -40 200 200 300 150]  
  [line-to doc 400 90]                         
  [stroke doc]

  [translate doc 0 200]

  ;; triangle
  [polygon doc '(100 0) '(50 100) '(150 100)]
  [stroke doc]

  ;; dashed circle
  [save doc]
  [translate doc 200 0]
  [circle doc 100 50 50]
  [dash doc 5 (hash 'space 10)]
  [stroke doc]
  [restore doc]

  ;; filled circle
  [save doc]
  [translate doc 400 0]
  [circle doc 100 50 50]
  [line-width doc 3]
  [fill-opacity doc 0.8]
  [fill-and-stroke doc "red" "#900"]
  [restore doc]

  [translate doc 0 200]

  ;; these examples are easier to see with a large line width
  [line-width doc 25]

  ;; line cap settings
  [line-cap doc 'butt]
  [move-to doc 50 20]
  [line-to doc 100 20]
  [stroke doc]
  [line-cap doc 'round]
  [move-to doc 150 20]
  [line-to doc 200 20]
  [stroke doc]

  ;; square line cap shown with a circle instead of a line so you can see it
  [line-cap doc 'square]
  [move-to doc 250 20]
  [circle doc 275 30 15]
  [stroke doc]

  ;; line join settings
  [line-join doc 'miter]
  [rect doc 50 100 50 50]
  [stroke doc]
  [line-join doc 'round]
  [rect doc 150 100 50 50]
  [stroke doc]
  [line-join doc 'bevel]
  [rect doc 250 100 50 50]
  [stroke doc])



(define-runtime-path this "test2rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test2crkt.pdf")
(make-doc that #t proc)
