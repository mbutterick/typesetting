#lang racket/base
(require sugar/unstable/class
         sugar/unstable/dict
         "../helper.rkt")

(require xenomorph)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/fpgm.js
|#

;; A list of instructions that are executed once when a font is first used.
;; These instructions are known as the font program. The main use of this table
;; is for the definition of functions that are used in many different glyph programs.


(define-subclass Struct (Rfpgm))

(define fpgm (+Rfpgm
               (dictify
                'instructions (+Array uint8))))


(test-module
 (require sugar/unstable/js
          sugar/unstable/port
          racket/class)
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define offset (· dir tables fpgm offset))
 (define len (· dir tables fpgm length))
 (check-equal? offset 4140)
 (check-equal? len 371)
 (check-equal? (pos ip 0) 0)
 (check-equal? (dict-ref (send fpgm decode (peek-bytes len offset ip)) 'instructions) '(184 0 0 44 75 184 0 9 80 88 177 1 1 142 89 184 1 255 133 184 0 68 29 185 0 9 0 3 95 94 45 184 0 1 44 32 32 69 105 68 176 1 96 45 184 0 2 44 184 0 1 42 33 45 184 0 3 44 32 70 176 3 37 70 82 88 35 89 32 138 32 138 73 100 138 32 70 32 104 97 100 176 4 37 70 32 104 97 100 82 88 35 101 138 89 47 32 176 0 83 88 105 32 176 0 84 88 33 176 64 89 27 105 32 176 0 84 88 33 176 64 101 89 89 58 45 184 0 4 44 32 70 176 4 37 70 82 88 35 138 89 32 70 32 106 97 100 176 4 37 70 32 106 97 100 82 88 35 138 89 47 253 45 184 0 5 44 75 32 176 3 38 80 88 81 88 176 128 68 27 176 64 68 89 27 33 33 32 69 176 192 80 88 176 192 68 27 33 89 89 45 184 0 6 44 32 32 69 105 68 176 1 96 32 32 69 125 105 24 68 176 1 96 45 184 0 7 44 184 0 6 42 45 184 0 8 44 75 32 176 3 38 83 88 176 64 27 176 0 89 138 138 32 176 3 38 83 88 35 33 176 128 138 138 27 138 35 89 32 176 3 38 83 88 35 33 184 0 192 138 138 27 138 35 89 32 176 3 38 83 88 35 33 184 1 0 138 138 27 138 35 89 32 176 3 38 83 88 35 33 184 1 64 138 138 27 138 35 89 32 184 0 3 38 83 88 176 3 37 69 184 1 128 80 88 35 33 184 1 128 35 33 27 176 3 37 69 35 33 35 33 89 27 33 89 68 45 184 0 9 44 75 83 88 69 68 27 33 33 89 45)))

