#lang pitfall/pdftest
(define-runtime-path this "test3crkt.pdf")

(check-true
 (let ([doc (make-object PDFDocument (hash 'compress #t))])
   (send doc pipe (open-output-file this #:exists 'replace))
   (send doc text "Hello world")
   (send doc end)))

(check-copy-equal? this)