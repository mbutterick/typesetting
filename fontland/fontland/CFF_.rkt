#lang racket/base
(require "racket.rkt")

(require xenomorph)
(provide CFF_)
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFFont.js
|#

;; the CFFFont object acts as the decoder for the `CFF ` table.


(define-subclass BufferT (RCFF_)
  )

(define CFF_ (+RCFF_))


(test-module)
