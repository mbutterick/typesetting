#lang racket
(require pitfall/document pitfall/params rackunit)

(test-mode #t)
(check-true
 (let ()
   (define doc (new PDFDocument))
   (send doc pipe (open-output-file "test0rkt.pdf" #:exists 'replace))
   (send doc end)))

(require racket/runtime-path)
(define-runtime-path this "test0rkt.pdf")
(define-runtime-path control "test0rkt copy.pdf")
(check-equal? (file->bytes this) (file->bytes control))