#lang fontkit/racket
(require restructure)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/loca.js
|#

(define-subclass RVersionedStruct (Rloca))

(define loca (make-object Rloca
               (λ (this) (hash-ref (send this _getTable 'head) 'indexToLocFormat)) 
               (dictify

                
                )))

(test-module
 )

