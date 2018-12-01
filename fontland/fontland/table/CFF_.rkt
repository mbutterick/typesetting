#lang racket/base
(require sugar/unstable/class)

(require xenomorph)
(provide CFF_)
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFFont.js
|#

;; the CFFFont object acts as the decoder for the `CFF ` table.
;; no CFF support yet



(define-subclass BufferT (RCFF_)
  )

(define CFF_ (+RCFF_))

