#lang racket/base
(require racket/class
         rackunit
         "document.rkt"
         "page.rkt"
         "reference.rkt"
         "core.rkt"
         sugar/unstable/js)
(define p (make-object PDFPage))
(check-equal? (· p size) "letter")
(check-equal? (· p layout) "portrait")
(check-equal? (· p margins) (margin 72 72 72 72))
(check-equal? (· p height) 792.0)
(check-equal? (· p width) 612.0)
(check-equal? (· p resources ProcSet) '("PDF" "Text" "ImageB" "ImageC" "ImageI"))

(check-equal? (· p dictionary Type) "Page")
(check-equal? (· p dictionary MediaBox)  '(0 0 612.0 792.0))
(check-true (is-a? (· p dictionary Contents) PDFReference))
(check-true (is-a? (· p dictionary Resources) PDFReference))
#;(check-true (is-a? (· p dictionary Parent) PDFReference))