#lang pitfall/pdftest
(define-runtime-path this "test1rkt.pdf")

(check-true
 (let ([doc (make-object PDFDocument (hash 'compress #f))])
   (send doc pipe (open-output-file this #:exists 'replace))

   ;; Draw a triangle and a circle
   (send* doc
     [save]
     [moveTo 100 150]
     [lineTo 100 250]
     [lineTo 200 250]
     [fill "#FF3300"])

   (send* doc
     [circle 280 200 50]
     [fill "#6600FF"])

   (send doc end)))

(check-copy-equal? this)
(check-pdfkit? this)