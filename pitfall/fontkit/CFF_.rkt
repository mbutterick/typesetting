#lang fontkit/racket
(require xenomorph)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/CFF.js
|#

(define CFF_ (+Array (+BufferT)))

(test-module)
