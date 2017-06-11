#lang fontkit/racket
(require restructure)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/prep.js
|#

(define-subclass RStruct (Rprep))

(define prep (make-object Rprep
               (dictify
                'controlValueProgram (make-object RArray uint8))))



