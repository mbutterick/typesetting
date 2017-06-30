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
      (unless (VoidPointer? val)
        (raise-argument-error 'Pointer:size "VoidPointer" val))

      (set! type (ref val 'type))
      (set! val (ref val 'value)))

    (when (and val ctx)
      (ref-set! ctx 'pointerSize (and (ref ctx 'pointerSize)
                                      (+ (ref ctx 'pointerSize) (send type size val parent)))))

    (send offsetType size))
                 

  (define/public (encode stream val [ctx #f])
    (define parent ctx)
    (define relative #f)
    (cond
      [(not val)
       (send offsetType encode stream (ref options 'nullValue))]
      [else
       (caseq (ref options 'type)
              [(local) (set! relative (ref ctx 'startOffset))]
              [(immediate) (set! relative (+ (· stream pos) (send offsetType size val parent)))]
              [(parent) (set! ctx (ref ctx 'parent))
                        (set! relative (ref ctx 'startOffset))]
              [else ; global
               (set! relative 0)
               (set! ctx (let loop ([ctx ctx])
                           (cond
                             [(ref ctx 'parent) => loop]
                             [else ctx])))])

       (when (ref options 'relativeTo)
         (increment! relative (relativeToGetter (ref parent 'val))))

       (send offsetType encode stream (- (ref ctx 'pointerOffset) relative))

       (define type_ type)
       (unless type_
         (unless (VoidPointer? val)
           (raise-argument-error 'Pointer:encode "VoidPointer" val))

         (set! type (ref val 'type))
         (set! val (ref val 'value)))

       (ref-set! ctx 'pointers (append (ref ctx 'pointers) (list (mhash 'type type
                                                                        'val val
                                                                        'parent parent))))
       (ref-set! ctx 'pointerOffset (+ (ref ctx 'pointerOffset) (send type size val parent)))])))


;; A pointer whose type is determined at decode time
(define-subclass object% (VoidPointer type value))
