#lang racket/base
(require sugar/unstable/class
         xenomorph)
(provide CFF_)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFFont.js
|#

;; the CFFFont object acts as the decoder for the `CFF ` table.
;; no CFF support yet

(define CFF_ (+xbuffer))

