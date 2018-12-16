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

(define xpointer%
  (class xenobase%
    (super-new)
    (init-field [(@offset-type offset-type)][(@type type)] [(@options options)])

    (define pointer-relative-to (dict-ref @options 'relative-to))
    (define allow-null (dict-ref @options 'allowNull)) 
    (define null-value (dict-ref @options 'nullValue))
    (define pointer-lazy? (dict-ref @options 'lazy))

    (define/augment (xxdecode port parent)
      (define offset (send @offset-type xxdecode port parent))
      (cond
        [(and allow-null (= offset null-value)) #f] ; handle null pointers
        [else
         (define relative (+ (case pointer-relative-to
                               [(local) (dict-ref parent '_startOffset)]
                               [(immediate) (- (pos port) (send @offset-type xxsize))]
                               [(parent) (dict-ref (dict-ref parent 'parent) '_startOffset)]
                               [(global) (or (dict-ref (find-top-parent parent) '_startOffset) 0)]
                               [else (error 'unknown-pointer-style)])))
         (define ptr (+ offset relative))
         (cond
           [@type (define (decode-value)
                    (define orig-pos (pos port))
                    (pos port ptr)
                    (begin0
                      (send @type xxdecode port parent)
                      (pos port orig-pos)))
                  (if pointer-lazy? (delay (decode-value)) (decode-value))]
           [else ptr])]))

    (define/augment (xxencode val-in port [parent #f])
      (unless parent ; todo: furnish default pointer context? adapt from Struct?
        (raise-argument-error 'xpointer-encode "valid pointer context" parent))
      (cond
        [val-in
         (define new-parent (case pointer-relative-to
                              [(local immediate) parent]
                              [(parent) (dict-ref parent 'parent)]
                              [(global) (find-top-parent parent)]
                              [else (error 'unknown-pointer-style)]))
         (define relative (+ (case pointer-relative-to
                               [(local parent) (dict-ref new-parent 'startOffset)]
                               [(immediate) (+ (pos port) (send @offset-type xxsize val-in parent))]
                               [(global) 0])))
         (send @offset-type xxencode (- (dict-ref new-parent 'pointerOffset) relative) port)
         (define-values (type val) (resolve-pointer @type val-in))
         (dict-update! new-parent 'pointers
                       (λ (ptrs) (append ptrs (list (mhasheq 'type type 'val val 'parent parent)))))
         (dict-set! new-parent 'pointerOffset
                    (+ (dict-ref new-parent 'pointerOffset) (send type xxsize val parent)))]
        [else (send @offset-type xxencode null-value port)]))

    (define/augment (xxsize [val-in #f] [parent #f])
      (define new-parent (case pointer-relative-to
                           [(local immediate) parent]
                           [(parent) (dict-ref parent 'parent)]
                           [(global) (find-top-parent parent)]
                           [else (error 'unknown-pointer-style)]))
      (define-values (type val) (resolve-pointer @type val-in))
      (when (and val new-parent)
        (dict-set! new-parent 'pointerSize
                   (and (dict-ref new-parent 'pointerSize #f)
                        (+ (dict-ref new-parent 'pointerSize) (send type xxsize val new-parent)))))
      (send @offset-type xxsize))))

(define (+xpointer [offset-arg #f] [type-arg #f]
                   #:offset-type [offset-kwarg #f]
                   #:type [type-kwarg #f]
                   #:relative-to [relative-to 'local]
                   #:lazy [lazy? #f]
                   #:allow-null [allow-null? #t]
                   #:null [null-value 0]
                   #:subclass [class xpointer%])
  (define valid-pointer-relatives '(local immediate parent global))
  (unless (memq relative-to valid-pointer-relatives)
    (raise-argument-error '+xpointer (format "~v" valid-pointer-relatives) relative-to))
  (define options (mhasheq 'relative-to relative-to
                           'lazy lazy?
                           'allowNull allow-null?
                           'nullValue null-value))
  (define type-in (or type-arg type-kwarg uint8))
  (new class
       [offset-type (or offset-arg offset-kwarg uint8)]
       [type (case type-in [(void) #f][else type-in])]
       [options options]))


;; A pointer whose type is determined at decode time
(define xvoid-pointer% (class xenobase%
                         (super-new)
                         (init-field type value)))
(define (+xvoid-pointer . args) (apply make-object xvoid-pointer% args))
(define (xvoid-pointer? x) (is-a? x xvoid-pointer%))
(define (xvoid-pointer-type x) (get-field type x))
(define (xvoid-pointer-value x) (get-field value x))
