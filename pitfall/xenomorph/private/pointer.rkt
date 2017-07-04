#lang reader (submod "racket.rkt" reader)
(require racket/undefined)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Pointer.coffee
|#

(define (resolve-void-pointer type val)
  (cond
    [type (values type val)]
    [(VoidPointer? val) (values (· val type) (· val value))]
    [else (raise-argument-error 'Pointer:size "VoidPointer" val)]))

(define (find-top-ctx ctx)
  (cond
    [(· ctx parent) => find-top-ctx]
    [else ctx]))

(define-subclass xenomorph-base% (Pointer offset-type type-in [options (mhasheq)])
  (field [type (and (not (eq? type-in 'void)) type-in)])
  (define pointer-style (or (· options type) 'local))
  (define allow-null (or (· options allowNull) #t)) 
  (define null-value (or (· options nullValue) 0))
  (define lazy (· options lazy))
  (define relative-getter-or-0 (or (· options relativeTo) (λ (ctx) 0))) ; changed this to a simple lambda

  (define/augment (decode port [ctx #f])
    (define offset (send offset-type decode port ctx))
    (cond
      [(and allow-null (= offset null-value)) #f] ; handle null pointers
      [else
       (define relative (+ (caseq pointer-style
                                  [(local) (· ctx _startOffset)]
                                  [(immediate) (- (pos port) (send offset-type size))]
                                  [(parent) (· ctx parent _startOffset)]
                                  [(global) (or (· (find-top-ctx ctx) _startOffset) 0)]
                                  [else (error 'unknown-pointer-style)])
                           (relative-getter-or-0 ctx)))
       (define ptr (+ offset relative))
       (cond
         [type (define val (void))
               (define (decode-value)
                 (cond
                   [(not (void? val)) val]
                   [else
                    (define orig-pos (pos port))
                    (pos port ptr)
                    (set! val (send type decode port ctx))
                    (pos port orig-pos)
                    val]))
               (if lazy
                   (LazyThunk decode-value)
                   (decode-value))]
         [else ptr])]))

  
  (define/augment (size [val #f] [ctx #f])
    (let*-values ([(parent) ctx]
                  [(ctx) (caseq pointer-style
                                [(local immediate) ctx]
                                [(parent) (· ctx parent)]
                                [(global) (find-top-ctx ctx)]
                                [else (error 'unknown-pointer-style)])]
                  [(type val) (resolve-void-pointer type val)])
      (when (and val ctx)
        (ref-set! ctx 'pointerSize (and (· ctx pointerSize)
                                        (+ (· ctx pointerSize) (send type size val parent)))))
      (send offset-type size)))
                 

  (define/augment (encode stream val [ctx #f])
    (if (not val)
        (send offset-type encode stream null-value)
        (let* ([parent ctx]
               [ctx (caseq pointer-style
                           [(local immediate) ctx]
                           [(parent) (· ctx parent)]
                           [(global) (find-top-ctx ctx)]
                           [else (error 'unknown-pointer-style)])]
               [relative (+ (caseq pointer-style
                                   [(local parent) (· ctx startOffset)]
                                   [(immediate) (+ (· stream pos) (send offset-type size val parent))]
                                   [(global) 0])
                            (relative-getter-or-0 (· parent val)))])
         
          (send offset-type encode stream (- (· ctx pointerOffset) relative))
         
          (let-values ([(type val) (resolve-void-pointer type val)])
            (ref-set! ctx 'pointers (append (· ctx pointers) (list (mhasheq 'type type
                                                                            'val val
                                                                            'parent parent))))
            (ref-set! ctx 'pointerOffset (+ (· ctx pointerOffset) (send type size val parent))))))))


;; A pointer whose type is determined at decode time
(define-subclass object% (VoidPointer type value))
