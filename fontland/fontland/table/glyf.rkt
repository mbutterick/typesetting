#lang racket/base
(require "../racket.rkt")

(require xenomorph)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/glyf.js
|#

(define-subclass Struct (Rglyf))

(define glyf (+Array (+BufferT)))

(test-module
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define offset (· dir tables glyf offset))
 (define len (· dir tables glyf length))
 (check-equal? offset 4620)
 (check-equal? len 34072)
 (set-port-position! ip 0)
 (define table-bytes (peek-bytes len offset ip)))
