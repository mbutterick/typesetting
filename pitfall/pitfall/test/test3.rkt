#lang pitfall/pdftest
(define-runtime-path this "test3rkt.pdf")

(check-true
 (let ([doc (new PDFDocument)])
   (send doc pipe (open-output-file this #:exists 'replace))
   (send doc text "Hello world")
   (send doc end)))

(check-copy-equal? this)