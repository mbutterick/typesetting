#lang br

(require pitfall/kit/document)

;; Create a new PDFDocument
(define doc (new PDFDocument))
(send doc pipe (open-output-file "test1rkt.pdf" #:exists 'replace))

;; Draw a triangle and a circle
(send (send (send (send (send doc save) moveTo 100 150) lineTo 100 250) lineTo 200 250) fill "#FF3300")

(send (send doc circle 280 200 50) fill "#6600FF")

;; render an SVG path
;; fill using the even-odd winding rule
(send (send (send (send (send doc scale 0.6) translate 470 -380) path "M 250,75 L 323,301 131,161 369,161 177,301 z") fill "red" "even-odd") restore)

(send doc end)