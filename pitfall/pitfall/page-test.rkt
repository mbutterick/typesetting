#lang racket/base
(require racket/class
         rackunit
         "document.rkt"
         "page.rkt"
         "reference.rkt"
         sugar/unstable/js)
(define p (make-object PDFPage (make-object PDFDocument)))
(check-equal? (· p size) "letter")
(check-equal? (· p layout) "portrait")
(check-equal? (· p margins) '#hasheq((right . 72) (bottom . 72) (left . 72) (top . 72)))
(check-equal? (· p height) 792.0)
(check-equal? (· p width) 612.0)
(check-equal? (· p resources payload ProcSet) '("PDF" "Text" "ImageB" "ImageC" "ImageI"))

(check-equal? (· p dictionary payload Type) "Page")
(check-equal? (· p dictionary payload MediaBox)  '(0 0 612.0 792.0))
(check-true (is-a? (· p dictionary payload Contents) PDFReference))
(check-true (is-a? (· p dictionary payload Resources) PDFReference))
(check-true (is-a? (· p dictionary payload Parent) PDFReference))