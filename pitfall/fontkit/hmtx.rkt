#lang fontkit/racket
(require restructure)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/hmtx.js
|#

(define-subclass Struct (Rhmtx))

(define HmtxEntry (make-object Struct
               (dictify
                'advance uint16be
                'bearing uint16be)))

(define hmtx (make-object Rhmtx
               (dictify
                'metrics uint16be
                'bearing uint16be)))



