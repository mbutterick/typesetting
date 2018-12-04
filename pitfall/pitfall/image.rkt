#lang racket/base
(require
  "helper.rkt"
  racket/class
  racket/contract
  "jpeg.rkt"
  "png.rkt")
(provide PDFImage-open)

#;(define PDFImage
  (class object%
    (super-new)

    (as-methods
     )))

(define/contract (PDFImage-open src label)
  (any/c any/c . -> . (or/c (is-a?/c PNG) (is-a?/c JPEG)))
  (define data (cond
                 [(isBuffer? src) src]
                 ;;else if src instanceof ArrayBuffer
                 ;;data = new Buffer(new Uint8Array(src))
                 [(regexp-match #rx"^data:.+;base64,(.*)$" src)
                  (void)] ;; base64 ; todo
                 [else (open-input-file src)]))
  (cond
    [(equal? (peek-bytes 2 0 data) (bytes #xff #xd8))
     (make-object JPEG data label)]
    [(equal? (peek-bytes 4 0 data) (apply bytes (cons #x89 (map char->integer '(#\P #\N #\G)))))
     (make-object PNG data label)]
    [else (raise-argument-error 'PDFImage-open "valid image format" src)]))
           
