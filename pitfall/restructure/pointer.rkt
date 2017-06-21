#lang restructure/racket
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Pointer.coffee
|#

(define-subclass RestructureBase (Pointer offsetType type [options (mhash)])

  (define/override (decode stream ctx)
    (report (file-position (· stream _port)))
    (define offset (send offsetType decode stream ctx))
    (define ptr offset)
    (report* offset ptr)
    (cond
      [type (define orig-pos (send stream pos))
            (send stream pos ptr)
            (define val (send type decode stream ctx))
            (send stream pos orig-pos)
            (report* options)
            val]
      [else ptr]))
            

  (define/override (encode stream val)
    (error 'Pointer-encode-not-done))

  (define/override (size val)
    (error 'Pointer-size-not-done))

  

  )