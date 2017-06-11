#lang fontkit/racket
(require restructure)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/fpgm.js
|#

;; A list of instructions that are executed once when a font is first used.
;; These instructions are known as the font program. The main use of this table
;; is for the definition of functions that are used in many different glyph programs.


(define-subclass RStruct (Rfpgm))

(define fpgm (make-object Rfpgm
               (dictify
                'instructions (make-object RArray uint8))))



