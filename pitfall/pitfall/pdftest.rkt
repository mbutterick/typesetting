#lang racket/base
(require
  (for-syntax racket/base)
  "core.rkt"
  racket/class
  racket/string
  br/define
  "check-pdf.rkt")

(provide check-copy-equal? check-pdfkit? make-doc)

(test-mode #t)

(require rackunit pitfall/document racket/runtime-path racket/class)
(provide (all-from-out  rackunit racket/runtime-path pitfall/document racket/class))

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
  (time
   (with-output-to-file ps
     (λ ()
       (define doc (make-object PDFDocument (hash 'compress compress?)))
       (proc doc)
       (send doc end))
     #:exists 'replace))
  (when test?
    #;(check-headers-equal? ps (this->control ps))
    (check-pdfs-equal? ps (this->control ps))
    (when pdfkit?
      #;(check-headers-equal? ps (this->pdfkit-control ps))
      (check-pdfs-equal? ps (this->pdfkit-control ps)))))
