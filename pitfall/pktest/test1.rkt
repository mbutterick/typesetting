#lang br

(require pitfall/kit/document pitfall/kit/helper)

;; Create a new PDFDocument
(define doc (new PDFDocument))
(send doc pipe (open-output-file "test1rkt.pdf" #:exists 'replace))

;; Draw a triangle and a circle
(send*/fold doc [save]
            [moveTo 100 150]
            [lineTo 100 250]
            [lineTo 200 250]
            [fill "#FF3300"])

(send*/fold doc [circle 280 200 50] [fill "#6600FF"])

(send doc end)