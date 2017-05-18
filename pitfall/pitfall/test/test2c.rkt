#lang pitfall/pdftest
(define-runtime-path this "test2crkt.pdf")

(check-true
 (let ([doc (make-object PDFDocument (hash 'compress #t))])
   (send doc pipe (open-output-file this #:exists 'replace))

   ;; curved path as bezier
   (send* doc
     [moveTo 0 20]
     [lineTo 100 160]
     [quadraticCurveTo 130 200 150 120]
     [bezierCurveTo 190 -40 200 200 300 150]  
     [lineTo 400 90]                         
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
     [lineWidth 3]
     [fillOpacity 0.8]
     [fillAndStroke "red" "#900"]
     [restore])

   (send* doc [translate 0 200])

   ;; these examples are easier to see with a large line width
   (send* doc [lineWidth 25])

   ;; line cap settings
   (send* doc [lineCap 'butt]
     [moveTo 50 20]
     [lineTo 100 20]
     [stroke]
     [lineCap 'round]
     [moveTo 150 20]
     [lineTo 200 20]
     [stroke])

   ;; square line cap shown with a circle instead of a line so you can see it
   (send* doc [lineCap 'square]
     [moveTo 250 20]
     [circle 275 30 15]
     [stroke])

   ;; line join settings
   (send* doc [lineJoin 'miter]
     [rect 50 100 50 50]
     [stroke]
     [lineJoin 'round]
     [rect 150 100 50 50]
     [stroke]
     [lineJoin 'bevel]
     [rect 250 100 50 50]
     [stroke])

   (send doc end)))

(check-copy-equal? this)