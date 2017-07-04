#lang fontkit/racket
(require xenomorph)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/prep.js
|#

(define-subclass Struct (Rprep))

(define prep (make-object Rprep
               (dictify
                'controlValueProgram (+Array uint8))))


(test-module
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define offset (· dir tables prep offset))
 (define len (· dir tables prep length))
 (check-equal? offset 4512)
 (check-equal? len 78)
 (set-port-position! ip 0)
 (define table-bytes #"\270\0\0+\0\272\0\1\0\1\0\2+\1\272\0\2\0\1\0\2+\1\277\0\2\0C\0007\0+\0\37\0\23\0\0\0\b+\0\277\0\1\0\200\0i\0R\0;\0#\0\0\0\b+\0\272\0\3\0\5\0\a+\270\0\0 E}i\30D")
 (check-equal? table-bytes (peek-bytes len offset ip))
 (define ds (open-input-bytes (peek-bytes len offset ip)))
 (check-equal? (dict-ref (decode prep ds) 'controlValueProgram) '(184 0 0 43 0 186 0 1 0 1 0 2 43 1 186 0 2 0 1 0 2 43 1 191 0 2 0 67 0 55 0 43 0 31 0 19 0 0 0 8 43 0 191 0 1 0 128 0 105 0 82 0 59 0 35 0 0 0 8 43 0 186 0 3 0 5 0 7 43 184 0 0 32 69 125 105 24 68)))
