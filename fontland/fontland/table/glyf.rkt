#lang racket/base
(require xenomorph)
(provide glyf)
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/glyf.js
|#

(define glyf (x:array #:type (x:buffer)))

(module+ test
 (require rackunit racket/serialize "../helper.rkt")
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define offset (hash-ref (hash-ref (hash-ref dir 'tables) 'glyf) 'offset))
 (define len (hash-ref (hash-ref (hash-ref dir 'tables) 'glyf) 'length))
 (check-equal? offset 4620)
 (check-equal? len 34072)
 (file-position ip 0)
 (define table-bytes (peek-bytes len offset ip)))
