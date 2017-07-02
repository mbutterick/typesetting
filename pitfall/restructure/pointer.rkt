#lang restructure/racket
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Pointer.coffee
|#

(define-subclass object% (Pointer offset-type type [options (mhash)])
  (when (eq? type 'void) (set! type #f))
  (hash-ref! options 'type 'local)
  (hash-ref! options 'allowNull #t)
  (hash-ref! options 'nullValue 0)
  (hash-ref! options 'lazy #f)
  (define relative-getter-or-0 (or (ref options 'relativeTo) (λ (ctx) 0))) ; changed this to a simple lambda

  (define/public (decode stream [ctx #f])
    (define offset (send offset-type decode stream ctx))
    (cond
      ;; handle NULL pointers
      [(and (ref options 'allowNull) (= offset (ref options 'nullValue))) #f]
      [else
       (define relative (+ (caseq (ref options 'type)
                                  [(local) (ref ctx '_startOffset)]
                                  [(immediate) (- (· stream pos) (send offset-type size))]
                                  [(parent) (ref* ctx 'parent '_startOffset)]
                                  [else (let loop ([ctx ctx])
                                          (cond
                                            [(· ctx parent) => loop]
                                            [(ref ctx '_startOffset)]
                                            [else 0]))])
                           (relative-getter-or-0 ctx)))
       (define ptr (+ offset relative))
       (cond
         ;; omitted: lazy pointer implementation
         [type (define orig-pos (· stream pos))
               (send stream pos ptr)
               (define val (send type decode stream ctx))
               (send stream pos orig-pos)
               val]
         [else ptr])]))

  
  (define/public (size [val #f] [ctx-in #f])
    (define parent ctx-in)
    (define ctx (caseq (ref options 'type)
                       [(local immediate) ctx-in]
                       [(parent) (· ctx-in parent)]
                       [(global) (let loop ([ctx ctx-in])
                                   (cond
                                     [(· ctx parent) => loop]
                                     [else ctx]))]
                       [else (error 'unknown-pointer-type)]))
    (unless type
      (unless (VoidPointer? val)
        (raise-argument-error 'Pointer:size "VoidPointer" val))
      (set! type (ref val 'type))
      (set! val (ref val 'value)))
    (when (and val ctx)
      (ref-set! ctx 'pointerSize (and (· ctx pointerSize)
                                      (+ (· ctx pointerSize) (send type size val parent)))))
    (send offset-type size))
                 

  (define/public (encode stream val [ctx #f])
    (define parent ctx)
    (define relative #f)
    (cond
      [(not val)
       (send offset-type encode stream (ref options 'nullValue))]
      [else
       (caseq (ref options 'type)
              [(local) (set! relative (ref ctx 'startOffset))]
              [(immediate) (set! relative (+ (· stream pos) (send offset-type size val parent)))]
              [(parent) (set! ctx (ref ctx 'parent))
                        (set! relative (ref ctx 'startOffset))]
              [(global) (set! relative 0)
                        (set! ctx (let loop ([ctx ctx])
                                    (cond
                                      [(ref ctx 'parent) => loop]
                                      [else ctx])))]
              [else (error 'unknown-pointer-type)])

       (increment! relative (relative-getter-or-0 (ref parent 'val)))
       (send offset-type encode stream (- (ref ctx 'pointerOffset) relative))

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
