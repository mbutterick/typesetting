#lang pitfall/racket
(require racket/runtime-path (for-syntax racket/base racket/format))
(provide isStandardFont standard-fonts)

(define (isStandardFont name) (hash-ref standard-fonts name #f))

(define-syntax (drps stx)
  (syntax-case stx ()
    [(_ hashid id ...)
     (let ([id-strings (map ~a (map syntax->datum (syntax->list #'(id ...))))])
       (with-syntax ([(path ...) (map (λ (d) (format "data/~a.afm" d)) id-strings)]
                     [(id-str ...) id-strings])
         #'(begin (define-runtime-path id path) ...
                  (define hashid (make-hash (list (cons id-str (λ () (file->string id))) ...))))))]))

(drps standard-fonts
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
