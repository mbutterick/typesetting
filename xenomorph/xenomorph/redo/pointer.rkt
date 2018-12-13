#lang debug racket/base
(require "helper.rkt"
         racket/dict
         racket/promise
         sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Pointer.coffee
|#

(define (find-top-parent parent)
  (cond
    [(dict-ref parent 'parent #f) => find-top-parent]
    [else parent]))

(define/post-decode (xpointer-decode xp [port-arg (current-input-port)] #:parent [parent #f])
  (define port (->input-port port-arg))
  (parameterize ([current-input-port port])
  (define offset (decode (xpointer-offset-type xp) #:parent parent))
  (cond
    [(and allow-null (= offset (null-value xp))) #f] ; handle null pointers
    [else
     (define relative (+ (case (pointer-style xp)
                           [(local) (dict-ref parent '_startOffset)]
                           [(immediate) (- (pos port) (size (xpointer-offset-type xp)))]
                           [(parent) (dict-ref (dict-ref parent 'parent) '_startOffset)]
                           [(global) (or (dict-ref (find-top-parent parent) '_startOffset) 0)]
                           [else (error 'unknown-pointer-style)])
                         ((relative-getter-or-0 xp) parent)))
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
             (set! val (decode (xpointer-type xp) #:parent parent))
             (pos port orig-pos)
             val]))
        (if (pointer-lazy? xp)
            (delay (decode-value))
            (decode-value))]
       [else ptr])])))

(define (resolve-void-pointer type val)
  (cond
    [type (values type val)]
    [(xvoid-pointer? val) (values (xvoid-pointer-type val) (xvoid-pointer-value val))]
    [else (raise-argument-error 'Pointer:size "VoidPointer" val)]))

(define/pre-encode (xpointer-encode xp val [port-arg (current-output-port)] #:parent [parent #f])
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (unless parent ; todo: furnish default pointer context? adapt from Struct?
    (raise-argument-error 'xpointer-encode "valid pointer context" parent))
  (parameterize ([current-output-port port])
  (if (not val)
      (encode (xpointer-offset-type xp) (null-value xp) port)
      (let* ([new-parent (case (pointer-style xp)
                    [(local immediate) parent]
                    [(parent) (dict-ref parent 'parent)]
                    [(global) (find-top-parent parent)]
                    [else (error 'unknown-pointer-style)])]
             [relative (+ (case (pointer-style xp)
                            [(local parent) (dict-ref new-parent 'startOffset)]
                            [(immediate) (+ (pos port) (size (xpointer-offset-type xp) val #:parent parent))]
                            [(global) 0])
                          ((relative-getter-or-0 xp) (dict-ref parent 'val #f)))])
        (encode (xpointer-offset-type xp) (- (dict-ref new-parent 'pointerOffset) relative))
        (let-values ([(type val) (resolve-void-pointer (xpointer-type xp) val)])
          (dict-set! new-parent 'pointers (append (dict-ref new-parent 'pointers)
                                           (list (mhasheq 'type type
                                                          'val val
                                                          'parent parent))))
          (dict-set! new-parent 'pointerOffset (+ (dict-ref new-parent 'pointerOffset) (size type val #:parent  parent)))))))
  (unless port-arg (get-output-bytes port)))

(define (xpointer-size xp [val #f] #:parent [parent #f])
  (let*-values ([(parent) (case (pointer-style xp)
                         [(local immediate) parent]
                         [(parent) (dict-ref parent 'parent)]
                         [(global) (find-top-parent parent)]
                         [else (error 'unknown-pointer-style)])]
                [(type val) (resolve-void-pointer (xpointer-type xp) val)])
    (when (and val parent)
      (dict-set! parent 'pointerSize (and (dict-ref parent 'pointerSize #f)
                                       (+ (dict-ref parent 'pointerSize) (size type val #:parent parent)))))
    (size (xpointer-offset-type xp))))

(struct xpointer xbase (offset-type type options) #:transparent
  #:methods gen:xenomorphic
  [(define decode xpointer-decode)
   (define encode xpointer-encode)
   (define size xpointer-size)])

(define (+xpointer offset-type type-in [options (mhasheq)])
  (xpointer offset-type (and (not (eq? type-in 'void)) type-in) options))

(define (pointer-style xp) (or (dict-ref (xpointer-options xp) 'type #f) 'local))
(define (allow-null xp) (or (dict-ref (xpointer-options xp) 'allowNull #f) #t)) 
(define (null-value xp) (or (dict-ref (xpointer-options xp) 'nullValue #f) 0))
(define (pointer-lazy? xp) (dict-ref (xpointer-options xp) 'lazy #f))
(define (relative-getter-or-0 xp) (or (dict-ref (xpointer-options xp) 'relativeTo #f) (λ (parent) 0))) ; changed this to a simple lambda

;; A pointer whose type is determined at decode time
(struct xvoid-pointer (type value) #:transparent)
(define +xvoid-pointer xvoid-pointer)
