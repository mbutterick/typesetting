#lang br

(require rackunit "document.rkt" "page.rkt")
(define p (make-object PDFPage (make-object PDFDocument)))
(get-field size p)
(get-field layout p)
(get-field margins p)
(get-field height p)
(get-field width p)
(get-field resources p)
(get-field data (get-field resources p))
(send p fonts)
(get-field data (get-field dictionary p))