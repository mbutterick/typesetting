#lang pitfall/racket
(require racket/runtime-path (for-syntax racket/base racket/path racket/syntax sugar/debug))
(provide isStandardFont standard-fonts)


(define (isStandardFont name) (hash-ref standard-fonts name #f))

(define-syntax (define-afm-table stx)
  (syntax-case stx ()
    [(_ HASH-ID FONT-ID ...)
     (with-syntax ([(PATH-STR ...) (map (λ (stx) (format "data/~a.afm" (syntax->datum stx))) (syntax->list #'(FONT-ID ...)))])
       #'(begin (define-runtime-path FONT-ID PATH-STR) ...
                (define HASH-ID (make-hash (list (cons (symbol->string 'FONT-ID) (procedure-rename (λ () (file->string FONT-ID)) 'FONT-ID)) ...)))))]))

(define-afm-table standard-fonts
  Courier-Bold
  Courier-BoldOblique
  Courier-Oblique
  Courier
  Helvetica-Bold
  Helvetica-BoldOblique
  Helvetica-Oblique
  Helvetica
  Symbol
  Times-Bold
  Times-BoldItalic
  Times-Italic
  Times-Roman
  ZapfDingbats)


(module+ test
  (require rackunit)
  (check-true (and (isStandardFont "Helvetica") #t))
  (check-true (and (isStandardFont "Courier") #t))
  (check-true (and (isStandardFont "ZapfDingbats") #t))
  (check-false (isStandardFont "Not A Font Name")))
