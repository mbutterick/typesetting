#lang fontkit/racket
(require restructure)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/prep.js
|#

(define-subclass Struct (Rprep))

(define prep (make-object Rprep
               (dictify
                'controlValueProgram (+Array uint8))))



