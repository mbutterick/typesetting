#lang pitfall/pdftest
(define-runtime-path this "test5crkt.pdf")

(check-true
 (let ([doc (make-object PDFDocument (hash 'compress #t))])
   (send doc pipe (open-output-file this #:exists 'replace))

   ;; # Set the font, draw some text, and embed an image
(send* doc
  [font "Times-Italic"]
  [fontSize 25]
  [text "Some text with an embedded font!" 100 100 (hash 'lineBreak #f)]
  [image "assets/test.png" 100 160 (hash 'width 412)]
  [image "assets/test.jpeg" 190 400 (hash 'height 300)]
 )

   
   (send doc end)))

;(check-copy-equal? this)
;(check-pdfkit? this)