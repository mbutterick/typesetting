#lang restructure/racket
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Pointer.coffee
|#

(define-subclass RestructureBase (Pointer offsetType type [scope 'local])
  (and (symbol? scope) (caseq scope
                              [(local parent) 'yay]
                              [else (raise-argument-error 'Pointer "local or parent" scope)]))

  (define/augride (decode stream ctx)
    #;(report* (· this starting-offset) (· this parent starting-offset))
    (define offset (send offsetType decode stream ctx))
    (define ptr (+ offset (caseq scope
                                 [(local) (· this parent starting-offset)]
                                 [(parent) (· this parent parent starting-offset)])))
    #;(report* offset ptr)
    (cond
      [type (define orig-pos (send stream pos))
            (send stream pos ptr)
            (define val (send type decode stream ctx))
            (send stream pos orig-pos)
            val]
      [else ptr]))
            

  (define/override (encode stream val)
    (error 'Pointer-encode-not-done))

  (define/override (size [val #f] [ctx #f])
    
    (report* this offsetType type)
    (report (send type size)))

  

  )