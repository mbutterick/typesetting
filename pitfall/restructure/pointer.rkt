#lang restructure/racket
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Pointer.coffee
|#

(define-subclass RestructureBase (Pointer offsetType type [scope 'local])
  (and (symbol? scope) (caseq scope
                              [(local parent immediate global) 'yay]
                              [else (raise-argument-error 'Pointer "local or parent or immediate" scope)]))

  (define/augride (decode stream [ctx #f])
    (define offset (send offsetType decode stream ctx))
    (report scope 'pointer-scope)
    (define relative (caseq scope
                            [(local) (· ctx _startOffset)]
                            [(parent) (· ctx parent _startOffset)]
                            [(immediate) (- (· stream pos) (send offsetType size))]
                            [(global) 
                                  (let loop ([c ctx])
                                    (cond
                                      [(· c parent) => loop]
                                      [else (or (· c starting-offset) 0)]))]))
    (report* (· this _startOffset) (and ctx (· ctx _startOffset)))
    (report* offset relative)
    (define ptr (+ offset relative))
    (report* ptr)
    (report (send offsetType size) 'size)
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
    (error 'Pointer-size-not-done)
    (report* this offsetType type (send type size)))

  

  )