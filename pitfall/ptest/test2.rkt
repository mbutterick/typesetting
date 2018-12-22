#lang racket/base
(require pitfall/pdftest)

(define (proc doc)

  ;; curved path as bezier
  (send* doc
    [move-to 0 20]
    [line-to 100 160]
    [quadratic-curve-to 130 200 150 120]
    [bezier-curve-to 190 -40 200 200 300 150]  
    [line-to 400 90]                         
    [stroke])

  (send* doc [translate 0 200])

  ;; triangle
  (send* doc
    [polygon '(100 0) '(50 100) '(150 100)]
    [stroke])

  ;; dashed circle
  (send* doc
    [save]
    [translate 200 0]
    [circle 100 50 50]
    [dash 5 (hash 'space 10)]
    [stroke]
    [restore])

  ;; filled circle
  (send* doc
    [save]
    [translate 400 0]
    [circle 100 50 50]
    [line-width 3]
    [fill-opacity 0.8]
    [fill-and-stroke "red" "#900"]
    [restore])

  (send* doc [translate 0 200])

  ;; these examples are easier to see with a large line width
  (send* doc [line-width 25])

  ;; line cap settings
  (send* doc [line-cap 'butt]
    [move-to 50 20]
    [line-to 100 20]
    [stroke]
    [line-cap 'round]
    [move-to 150 20]
    [line-to 200 20]
    [stroke])

  ;; square line cap shown with a circle instead of a line so you can see it
  (send* doc [line-cap 'square]
    [move-to 250 20]
    [circle 275 30 15]
    [stroke])

  ;; line join settings
  (send* doc [line-join 'miter]
    [rect 50 100 50 50]
    [stroke]
    [line-join 'round]
    [rect 150 100 50 50]
    [stroke]
    [line-join 'bevel]
    [rect 250 100 50 50]
    [stroke]))



(define-runtime-path this "test2rkt.pdf")
(make-doc this #f proc)

(define-runtime-path that "test2crkt.pdf")
(make-doc that #t proc)
