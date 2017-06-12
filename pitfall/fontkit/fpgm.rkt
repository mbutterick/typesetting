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


(define-subclass Struct (fpgm%))

(define fpgm (make-object fpgm%
               (dictify
                'instructions (make-object Array uint8))))



