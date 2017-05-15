#lang racket

(require pitfall/document pitfall/helper pitfall/params rackunit)

(require racket/runtime-path)
(define-runtime-path this "test1rkt.pdf")
(define-runtime-path control "test1rkt copy.pdf")

;; Create a new PDFDocument
(test-mode #t)
(check-true
 (let ([doc (new PDFDocument)])
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

(check-equal? (file->bytes this) (file->bytes control))