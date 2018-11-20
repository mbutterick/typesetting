#lang racket/base
(require "racket.rkt" "check-pdf.rkt")

(provide (all-from-out "racket.rkt"))
(provide check-copy-equal? check-pdfkit? make-doc)

(test-mode #t)

(r+p rackunit racket/runtime-path pitfall/document)

(define (this->control this) (path-add-extension this #"" #" copy."))

(define (this->pdfkit-control this)
  (string->path (string-replace ((if (string? this) values path->string) this) "rkt." ".")))

(module+ test
  (require rackunit)
  (check-equal? (this->pdfkit-control (string->path "test1crkt.pdf")) (string->path "test1c.pdf")))


(define-macro (check-copy-equal? THIS)
  (syntax/loc caller-stx (check-true (for/and ([b1 (in-input-port-bytes (open-input-file THIS))]
                                               [b2 (in-input-port-bytes (open-input-file (this->control THIS)))])
                                              (equal? b1 b2)))))


(define-syntax-rule (check-pdfkit? this)
  (check-equal? (file-size this) (file-size (this->pdfkit-control this))))

(define (make-doc ps [compress? #false] [proc (λ (doc) doc)] #:test [test? #t] #:pdfkit [pdfkit? #t])
  (define doc (make-object PDFDocument (hash 'compress compress?)))
  (send doc pipe (open-output-file ps #:exists 'replace))
  (proc doc)
  (send doc end)
  (when test?
    (check-pdfs-equal? ps (this->control ps))
    (when pdfkit?
      (check-pdfs-equal? ps (this->pdfkit-control ps)))))
