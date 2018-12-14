#lang racket/base
(require sugar/unstable/class
         "../helper.rkt"
         xenomorph)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/glyf.js
|#

(define glyf (+xarray #:type (+xbuffer)))

(test-module
 (require sugar/unstable/js
          sugar/unstable/port)
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define offset (· dir tables glyf offset))
 (define len (· dir tables glyf length))
 (check-equal? offset 4620)
 (check-equal? len 34072)
 (set-port-position! ip 0)
 (define table-bytes (peek-bytes len offset ip)))
