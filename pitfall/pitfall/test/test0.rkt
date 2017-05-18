#lang pitfall/pdftest
(define-runtime-path this "test0rkt.pdf")

(check-true
 (let ()
   (define doc (make-object PDFDocument (hash 'compress #f)))
   (send doc pipe (open-output-file this #:exists 'replace))
   (send doc end)))

(check-copy-equal? this)