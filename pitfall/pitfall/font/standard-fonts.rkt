#lang pitfall/racket
(require racket/runtime-path)
(provide isStandardFont standard-fonts)

(define (isStandardFont name)
  (hash-ref standard-fonts name #f))

(define-runtime-path Helvetica "data/Helvetica.afm")

(define standard-fonts
  (hash "Helvetica" (λ () (file->string Helvetica))))

(module+ test
  (require rackunit)
  (check-true (and (isStandardFont "Helvetica") #t))
  (check-false (isStandardFont "Not A Font Name")))