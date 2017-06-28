#lang restructure/racket
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Pointer.coffee
|#

(define-subclass object% (Pointer offsetType type [scope 'local])
  (when (eq? type 'void) (set! type #f))
  
  (and (symbol? scope) (caseq scope
                              [(local parent immediate global) 'yay]
                              [else (raise-argument-error 'Pointer "local or parent or immediate" scope)]))

  (define/public (decode stream [ctx #f])
    (define offset (send offsetType decode stream ctx))
    (report scope 'pointer-scope)
    (define relative (caseq scope
                            [(local) #;(when (and (· ctx res _startOffset) (· ctx _startOffset)
                                                (not (= (· ctx res _startOffset) (· ctx _startOffset))))
                                       (report* ctx (· ctx res _startOffset) (· ctx _startOffset))
                                       (error 'bazongas))
                                     (· ctx res _startOffset)]
                            [(parent) (· ctx res parent res _startOffset)]
                            [(immediate) (- (· stream pos) (send offsetType size))]
                            [(global) 
                             (let loop ([c ctx])
                               (cond
                                 [(· c parent) => loop]
                                 [(· c _startOffset)]
                                 [else 0]))]))
    (report* this (· this _startOffset)
             (and (· this res) (· this res _startOffset))
             ctx
             (and (· ctx res) (· ctx res _startOffset))) 
    #;(when (and ctx (· ctx res _startOffset) (= (· ctx res _startOffset) 1012)) (error 'stop))
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

  
  (define/public (size [val #f] [ctx #f])
    (error 'Pointer-size-not-done)
    (report* this offsetType type (send type size)))
            

  (define/public (encode stream val)
    (error 'Pointer-encode-not-done))


  

  )