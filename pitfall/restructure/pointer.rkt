#lang restructure/racket
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Pointer.coffee
|#

(define-subclass RestructureBase (Pointer offsetType type [scope 'local])
  (and (symbol? scope) (caseq scope
                              [(local parent grandparent immediate) 'yay]
                              [else (raise-argument-error 'Pointer "local or parent" scope)]))

  (define/augride (decode stream ctx)
    (define offset (send offsetType decode stream ctx))
    (define ptr (+ offset (caseq scope
                                 [(immediate) (· this starting-offset)]
                                 [(local) (· this parent starting-offset)]
                                 [(parent) (· this parent parent starting-offset)]
                                 [(grandparent) (· this parent parent parent starting-offset)])))
    (report* ptr (send offsetType size))
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
    
    (report* this offsetType type (send type size)))

  

  )