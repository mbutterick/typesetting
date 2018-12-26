#lang racket/base
(require
  racket/class
  "jpeg.rkt"
  "png.rkt")
(provide PDFImage-open)

(define (PDFImage-open src label)
  (define data (cond
                 [(bytes? src) (open-input-bytes src)]
                 [(regexp-match #rx"^data:.+;base64,(.*)$" src) (void)] ;; base64 ; todo
                 [else (open-input-file src)]))
  (cond
      [(equal? (peek-bytes 2 0 data) (bytes #xff #xd8)) (make-object JPEG data label)]
      [(equal? (peek-bytes 4 0 data) (apply bytes (map char->integer '(#\u0089 #\P #\N #\G)))) (make-png data label)]
      [else (raise-argument-error 'PDFImage-open "valid image format" src)]))
           
