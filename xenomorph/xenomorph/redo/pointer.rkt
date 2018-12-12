#lang debug racket/base
(require "helper.rkt"
         racket/dict
         sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Pointer.coffee
|#

(define (find-top-ctx ctx)
  (cond
    [(dict-ref ctx 'parent #f) => find-top-ctx]
    [else ctx]))

(define (xpointer-decode xp [port-arg (current-input-port)] #:parent [ctx #f])
  (define port (->input-port port-arg))
  (parameterize ([current-input-port port])
  (define offset (decode (xpointer-offset-type xp) #:parent ctx))
  (cond
    [(and allow-null (= offset (null-value xp))) #f] ; handle null pointers
    [else
     (define relative (+ (case (pointer-style xp)
                           [(local) (dict-ref ctx '_startOffset)]
                           [(immediate) (- (pos port) (size (xpointer-offset-type xp)))]
                           [(parent) (dict-ref (dict-ref ctx 'parent) '_startOffset)]
                           [(global) (or (dict-ref (find-top-ctx ctx) '_startOffset) 0)]
                           [else (error 'unknown-pointer-style)])
                         ((relative-getter-or-0 xp) ctx)))
     (define ptr (+ offset relative))
     (cond
       [(xpointer-type xp)
        (define val (void))
        (define (decode-value)
          (cond
            [(not (void? val)) val]
            [else
             (define orig-pos (pos port))
             (pos port ptr)
             (set! val (decode (xpointer-type xp) #:parent ctx))
             (pos port orig-pos)
             val]))
        (if (lazy xp)
            (lazy-thunk decode-value)
            (decode-value))]
       [else ptr])])))

(define (resolve-void-pointer type val)
  (cond
    [type (values type val)]
    [(xvoid-pointer? val) (values (xvoid-pointer-type val) (xvoid-pointer-value val))]
    [else (raise-argument-error 'Pointer:size "VoidPointer" val)]))

(define (xpointer-encode xp val [port-arg (current-output-port)] #:parent [ctx #f])
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (unless ctx ; todo: furnish default pointer context? adapt from Struct?
    (raise-argument-error 'xpointer-encode "valid pointer context" ctx))
  (parameterize ([current-output-port port])
  (if (not val)
      (encode (xpointer-offset-type xp) (null-value xp) port)
      (let* ([parent ctx]
             [ctx (case (pointer-style xp)
                    [(local immediate) ctx]
                    [(parent) (dict-ref ctx 'parent)]
                    [(global) (find-top-ctx ctx)]
                    [else (error 'unknown-pointer-style)])]
             [relative (+ (case (pointer-style xp)
                            [(local parent) (dict-ref ctx 'startOffset)]
                            [(immediate) (+ (pos port) (size (xpointer-offset-type xp) val parent))]
                            [(global) 0])
                          ((relative-getter-or-0 xp) (dict-ref parent 'val #f)))])
        (encode (xpointer-offset-type xp) (- (dict-ref ctx 'pointerOffset) relative))
        (let-values ([(type val) (resolve-void-pointer (xpointer-type xp) val)])
          (dict-set! ctx 'pointers (append (dict-ref ctx 'pointers)
                                           (list (mhasheq 'type type
                                                          'val val
                                                          'parent parent))))
          (dict-set! ctx 'pointerOffset (+ (dict-ref ctx 'pointerOffset) (size type val parent)))))))
  (unless port-arg (get-output-bytes port)))

(define (xpointer-size xp [val #f] [ctx #f])
  (let*-values ([(parent) ctx]
                [(ctx) (case (pointer-style xp)
                         [(local immediate) ctx]
                         [(parent) (dict-ref ctx 'parent)]
                         [(global) (find-top-ctx ctx)]
                         [else (error 'unknown-pointer-style)])]
                [(type val) (resolve-void-pointer (xpointer-type xp) val)])
    (when (and val ctx)
      (dict-set! ctx 'pointerSize (and (dict-ref ctx 'pointerSize #f)
                                       (+ (dict-ref ctx 'pointerSize) (size type val parent)))))
    (size (xpointer-offset-type xp))))

(struct xpointer (offset-type type options) #:transparent
  #:methods gen:xenomorphic
  [(define decode xpointer-decode)
   (define encode xpointer-encode)
   (define size xpointer-size)])

(define (+xpointer offset-type type-in [options (mhasheq)])
  (xpointer offset-type (and (not (eq? type-in 'void)) type-in) options))

(define (pointer-style xp) (or (dict-ref (xpointer-options xp) 'type #f) 'local))
(define (allow-null xp) (or (dict-ref (xpointer-options xp) 'allowNull #f) #t)) 
(define (null-value xp) (or (dict-ref (xpointer-options xp) 'nullValue #f) 0))
(define (lazy xp) (dict-ref (xpointer-options xp) 'lazy #f))
(define (relative-getter-or-0 xp) (or (dict-ref (xpointer-options xp) 'relativeTo #f) (λ (ctx) 0))) ; changed this to a simple lambda

;; A pointer whose type is determined at decode time
(struct xvoid-pointer (type value) #:transparent)
(define +xvoid-pointer xvoid-pointer)