#lang pitfall/racket
(provide (all-from-out pitfall/racket))
(provide check-copy-equal? check-pdfkit?)

(test-mode #t)

(r+p rackunit racket/runtime-path pitfall/document)

(define (this->control this) (path-add-extension this #"" #" copy."))

(define (this->pdfkit-control this)
  (string->path (string-replace (path->string this) "rkt." ".")))

(module+ test
  (require rackunit)
  (check-equal? (this->pdfkit-control (string->path "test1crkt.pdf")) (string->path "test1c.pdf")))


(define-syntax-rule (check-copy-equal? this)
  (check-equal? (file->bytes this) (file->bytes (this->control this))))


(define-syntax-rule (check-pdfkit? this)
  (check-equal? (bytes-length (file->bytes this))
                (bytes-length (file->bytes (this->pdfkit-control this)))))


(module reader syntax/module-reader
  #:language 'pitfall/pdftest
  #:read read
  #:read-syntax read-syntax)