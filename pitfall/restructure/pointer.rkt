#lang restructure/racket
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Pointer.coffee
|#

(define-subclass object% (Pointer offsetType type [options (mhash)])
  (when (eq? type 'void) (set! type #f))
  (hash-ref! options 'type 'local)
  (hash-ref! options 'allowNull #t)
  (hash-ref! options 'nullValue 0)
  (hash-ref! options 'lazy #f)
  (define relativeToGetter (ref options 'relativeTo)) ; change this to a simple lambda

  (define/public (decode stream [ctx #f])
    (define offset (send offsetType decode stream ctx))

    (cond
      ;; handle NULL pointers
      [(and (eq? offset (ref options 'nullValue)) (ref options 'allowNull)) #f]
      [else
       (define relative (caseq (ref options 'type)
                               [(local) (ref ctx '_startOffset)]
                               [(immediate) (- (· stream pos) (send offsetType size))]
                               [(parent) (ref* ctx 'parent '_startOffset)]
                               [else (let loop ([ctx ctx])
                                       (cond
                                         [(· ctx parent) => loop]
                                         [(ref ctx '_startOffset)]
                                         [else 0]))]))

       (when (ref options 'relativeTo)
         ; relativeToGetter only defined if 'relativeTo key exists, so this is safe
         (increment! relative (relativeToGetter ctx)))

       (define ptr (+ offset relative))

       (cond
         [type (define val #f)
               (define (decodeValue)
                 (cond
                   [val]
                   [else (define pos (· stream pos))
                         (send stream pos ptr)
                         (define val (send type decode stream ctx))
                         (send stream pos pos)
                         val]))

               ;; skip lazy pointer chores

               (decodeValue)]
         [else ptr])]))

  
  (define/public (size [val #f] [ctx #f])
    (define parent ctx)
    (caseq (ref options 'type)
           [(local immediate) (void)]
           [(parent) (set! ctx (ref ctx 'parent))]
           [else ; global
            (set! ctx (let loop ([ctx ctx])
                        (cond
                          [(ref ctx 'parent) => loop]
                          [else ctx])))])

    (define type_ type)
    (unless type_
      ; todo: uncomment when VoidPointer class is ready
      #;(unless (VoidPointer? val)
          (raise-argument-error 'Pointer:size "VoidPointer" val))

      (set! type (ref val 'type))
      (set! val (ref val 'value)))

    (when (and val ctx)
      (ref-set! ctx 'pointerSize (+ (ref ctx 'pointerSize) (send type size val parent))))

    (send offsetType size))
                 

  #;(define/public (encode stream val)
      (error 'Pointer-encode-not-done))


  

  )