#lang debug racket/base
(require "helper.rkt"
         "number.rkt"
         racket/dict
         racket/class
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

(define (resolve-pointer type val)
  (cond
    [type (values type val)]
    [(xvoid-pointer? val) (values (xvoid-pointer-type val) (xvoid-pointer-value val))]
    [else (raise-argument-error 'Pointer:size "VoidPointer" val)]))

(define x:pointer%
  (class x:enobase%
    (super-new)
    (init-field [(@offset-type offset-type)]
                [(@type type)]
                [(@pointer-relative-to pointer-relative-to)]
                [(@allow-null? allow-null?)]
                [(@null-value null-value)]
                [(@pointer-lazy? pointer-lazy?)])

    (define/augment (x:decode port parent)
      (define offset (send @offset-type x:decode port parent))
      (cond
        [(and @allow-null? (= offset @null-value)) #f] ; handle null pointers
        [else
         (define relative (+ (case @pointer-relative-to
                               [(local) (dict-ref parent '_startOffset)]
                               [(immediate) (- (pos port) (send @offset-type x:size))]
                               [(parent) (dict-ref (dict-ref parent 'parent) '_startOffset)]
                               [(global) (or (dict-ref (find-top-parent parent) '_startOffset) 0)]
                               [else (error 'unknown-pointer-style)])))
         (define ptr (+ offset relative))
         (cond
           [@type (define (decode-value)
                    (define orig-pos (pos port))
                    (pos port ptr)
                    (begin0
                      (send @type x:decode port parent)
                      (pos port orig-pos)))
                  (if @pointer-lazy? (delay (decode-value)) (decode-value))]
           [else ptr])]))

    (define/augment (x:encode val-in port [parent #f])
      (unless parent ; todo: furnish default pointer context? adapt from Struct?
        (raise-argument-error 'xpointer-encode "valid pointer context" parent))
      (cond
        [val-in
         (define new-parent (case @pointer-relative-to
                              [(local immediate) parent]
                              [(parent) (dict-ref parent 'parent)]
                              [(global) (find-top-parent parent)]
                              [else (error 'unknown-pointer-style)]))
         (define relative (+ (case @pointer-relative-to
                               [(local parent) (dict-ref new-parent 'startOffset)]
                               [(immediate) (+ (pos port) (send @offset-type x:size val-in parent))]
                               [(global) 0])))
         (send @offset-type x:encode (- (dict-ref new-parent 'pointerOffset) relative) port)
         (define-values (type val) (resolve-pointer @type val-in))
         (dict-update! new-parent 'pointers
                       (λ (ptrs) (append ptrs (list (mhasheq 'type type 'val val 'parent parent)))))
         (dict-set! new-parent 'pointerOffset
                    (+ (dict-ref new-parent 'pointerOffset) (send type x:size val parent)))]
        [else (send @offset-type x:encode @null-value port)]))

    (define/augment (x:size [val-in #f] [parent #f])
      (define new-parent (case @pointer-relative-to
                           [(local immediate) parent]
                           [(parent) (dict-ref parent 'parent)]
                           [(global) (find-top-parent parent)]
                           [else (error 'unknown-pointer-style)]))
      (define-values (type val) (resolve-pointer @type val-in))
      (when (and val new-parent)
        (dict-set! new-parent 'pointerSize
                   (and (dict-ref new-parent 'pointerSize #f)
                        (+ (dict-ref new-parent 'pointerSize) (send type x:size val new-parent)))))
      (send @offset-type x:size))))

(define (+xpointer [offset-arg #f] [type-arg #f]
                   #:offset-type [offset-kwarg #f]
                   #:type [type-kwarg #f]
                   #:relative-to [pointer-relative-to 'local]
                   #:lazy [pointer-lazy? #f]
                   #:allow-null [allow-null? #t]
                   #:null [null-value 0]
                   #:pre-encode [pre-proc #f]
                   #:post-decode [post-proc #f])
  (define valid-pointer-relatives '(local immediate parent global))
  (unless (memq pointer-relative-to valid-pointer-relatives)
    (raise-argument-error '+xpointer (format "~v" valid-pointer-relatives) pointer-relative-to))
  (define type-in (or type-arg type-kwarg uint8))
  (new (generate-subclass x:pointer% pre-proc post-proc)
       [offset-type (or offset-arg offset-kwarg uint8)]
       [type (case type-in [(void) #f][else type-in])]
       [pointer-relative-to pointer-relative-to]
       [pointer-lazy? pointer-lazy?]
       [allow-null? allow-null?]
       [null-value null-value]))

;; A pointer whose type is determined at decode time
(define x:void-pointer% (class x:enobase%
                         (super-new)
                         (init-field type value)))
(define (+xvoid-pointer . args) (apply make-object x:void-pointer% args))
(define (xvoid-pointer? x) (is-a? x x:void-pointer%))
(define (xvoid-pointer-type x) (get-field type x))
(define (xvoid-pointer-value x) (get-field value x))
