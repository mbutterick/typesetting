#lang racket/base
(require "../racket.rkt")

(require xenomorph)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/cvt.js
|#

(define-subclass Struct (Rcvt_))

(define cvt_ (+Rcvt_
               (dictify
                'controlValues (+Array int16be))))


(test-module
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define offset (· dir tables cvt_ offset))
 (define len (· dir tables cvt_ length))
 (check-equal? offset 4592)
 (check-equal? len 26)
 (set-port-position! ip 0)
 (define table-bytes #"\0\24\0+\0S\0\0\0\20\377&\0\0\1\341\0\v\2\237\0\22\2\340\0\b")
 (check-equal? table-bytes (peek-bytes len offset ip))
 (define ds (open-input-bytes (peek-bytes len offset ip)))
 (define cvt-array '(20 43 83 0 16 -218 0 481 11 671 18 736 8))
 (check-equal? (dict-ref (decode cvt_ ds) 'controlValues) cvt-array)
 (check-equal? (encode cvt_ (mhash 'controlValues cvt-array) #f) table-bytes))
