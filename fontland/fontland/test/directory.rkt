#lang racket
(require rackunit racket/runtime-path fontland)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/test/directory.js
|#

(define-runtime-path open-sans-ttf "data/OpenSans/OpenSans-Regular.ttf")

(define font (open-font open-sans-ttf))

(test-case
 "decodes SFNT directory values correctly"
 (define dir (font-directory font))
 (check-equal? (hash-ref dir 'numTables) 19)
 (check-equal? (hash-ref dir 'searchRange) 256)
 (check-equal? (hash-ref dir 'entrySelector) 4)
 (check-equal? (hash-ref dir 'rangeShift) 48))

(test-case 
 "numTables matches table collection"
 (define dir (font-directory font))
 (check-equal? (length (hash-keys (hash-ref dir 'tables))) (hash-ref dir 'numTables)))
