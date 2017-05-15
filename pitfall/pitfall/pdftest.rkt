#lang pitfall/racket
(provide (all-from-out pitfall/racket))

(test-mode #t)

(r+p rackunit racket/runtime-path pitfall/document)

(define (this->control this) (path-add-extension this #"" #" copy."))

(provide check-copy-equal?)
(define-syntax-rule (check-copy-equal? this)
  (check-equal? (file->bytes this) (file->bytes (this->control this))))

(module reader syntax/module-reader
  #:language 'pitfall/pdftest
  #:read read
  #:read-syntax read-syntax)