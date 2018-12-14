#lang debug racket/base
(require "helper.rkt"
         "number.rkt"
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
       (define relative (+ (case (pointer-relative-to xp)
                             [(local) (dict-ref parent '_startOffset)]
                             [(immediate) (- (pos port) (size (xpointer-offset-type xp)))]
                             [(parent) (dict-ref (dict-ref parent 'parent) '_startOffset)]
                             [(global) (or (dict-ref (find-top-parent parent) '_startOffset) 0)]
                             [else (error 'unknown-pointer-style)])))
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
        (let* ([new-parent (case (pointer-relative-to xp)
                             [(local immediate) parent]
                             [(parent) (dict-ref parent 'parent)]
                             [(global) (find-top-parent parent)]
                             [else (error 'unknown-pointer-style)])]
               [relative (+ (case (pointer-relative-to xp)
                              [(local parent) (dict-ref new-parent 'startOffset)]
                              [(immediate) (+ (pos port) (size (xpointer-offset-type xp) val #:parent parent))]
                              [(global) 0]))])
          (encode (xpointer-offset-type xp) (- (dict-ref new-parent 'pointerOffset) relative))
          (let-values ([(type val) (resolve-void-pointer (xpointer-type xp) val)])
            (dict-set! new-parent 'pointers (append (dict-ref new-parent 'pointers)
                                                    (list (mhasheq 'type type
                                                                   'val val
                                                                   'parent parent))))
            (dict-set! new-parent 'pointerOffset (+ (dict-ref new-parent 'pointerOffset) (size type val #:parent  parent)))))))
  (unless port-arg (get-output-bytes port)))

(define (xpointer-size xp [val #f] #:parent [parent #f])
  (let*-values ([(parent) (case (pointer-relative-to xp)
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

(define (+xpointer [offset-arg #f] [type-arg #f]
                   #:offset-type [offset-kwarg #f]
                   #:type [type-kwarg #f]
                   #:relative-to [relative-to 'local]
                   #:lazy [lazy? #f]
                   #:allow-null [allow-null? #t]
                   #:null [null-value 0])
  (define valid-pointer-relatives '(local immediate parent global))
  (unless (memq relative-to valid-pointer-relatives)
    (raise-argument-error '+xpointer (format "~v" valid-pointer-relatives) relative-to))
  (define options (mhasheq 'relative-to relative-to
                           'lazy lazy?
                           'allowNull allow-null?
                           'nullValue null-value))
  (define offset-type (or offset-arg offset-kwarg uint8))
  (define type-in (or type-arg type-kwarg uint8))
  (xpointer offset-type (case type-in [(void) #f][else type-in]) options))

(define (pointer-relative-to xp) (dict-ref (xpointer-options xp) 'relative-to))
(define (allow-null xp) (dict-ref (xpointer-options xp) 'allowNull)) 
(define (null-value xp) (dict-ref (xpointer-options xp) 'nullValue))
(define (pointer-lazy? xp) (dict-ref (xpointer-options xp) 'lazy))

;; A pointer whose type is determined at decode time
(struct xvoid-pointer (type value) #:transparent)
(define +xvoid-pointer xvoid-pointer)
