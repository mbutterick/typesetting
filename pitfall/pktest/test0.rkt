#lang br

(require pitfall/kit/document)

(define doc (new PDFDocument))
(send doc pipe (open-output-file "test0rkt.pdf" #:exists 'replace))
(send doc end)