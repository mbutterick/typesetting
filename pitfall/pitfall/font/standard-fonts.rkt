#lang pitfall/racket
(require racket/runtime-path)
(provide isStandardFont STANDARD_FONTS)

(define (isStandardFont name)
  (hash-ref STANDARD_FONTS name #f))

(define-runtime-path Helvetica "data/Helvetica.afm")

(define STANDARD_FONTS
  (hash "Helvetica" (λ () (file->string Helvetica))))

(module+ test
  (require rackunit)
  (check-true (and (isStandardFont "Helvetica") #t))
  (check-false (isStandardFont "Not A Font Name")))