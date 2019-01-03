#lang racket/base
(require racket/class
         rackunit
         racket/dict
         "pdf.rkt"
         "page.rkt"
         "reference.rkt"
         "core.rkt"
         sugar/unstable/js)
(define p (make-page))
(check-equal? ($page-size p) "letter")
(check-equal? ($page-layout p) "portrait")
(check-equal? ($page-margins p) (margin 72 72 72 72))
(check-equal? ($page-height p) 792.0)
(check-equal? ($page-width p) 612.0)
(check-equal? (dict-ref ($page-resources p) 'ProcSet) '(PDF Text ImageB ImageC ImageI))

(check-equal? (dict-ref ($page-ref p) 'Type) 'Page)
(check-equal? (dict-ref ($page-ref p) 'MediaBox)  '(0 0 612.0 792.0))
(check-true ($ref? (dict-ref ($page-ref p) 'Contents)))
(check-true ($ref? (dict-ref ($page-ref p) 'Resources)))