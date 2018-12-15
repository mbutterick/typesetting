#lang debug racket/base
(require racket/dict
         racket/class
         racket/sequence
         "helper.rkt"
         "number.rkt"
         "util.rkt"
         sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Array.coffee
|#

(define xarray-base%
  (class xenobase%
    (super-new)
    (init-field type len)
    (unless (xenomorphic-type? type)
      (raise-argument-error '+xarray "xenomorphic type" type))
    (unless (length-resolvable? len)
      (raise-argument-error '+xarray "length-resolvable?" len))))

(define xarray%
  (class xarray-base%
    (super-new)
    (init-field length-type)
    (unless (memq length-type '(bytes count))
      (raise-argument-error '+xarray "'bytes or 'count" length-type))

    (inherit-field type len)

    (define/augment (xxdecode port parent)
      (define new-parent (if (xint? len)
                             (mhasheq 'parent parent
                                      '_startOffset (pos port)
                                      '_currentOffset 0
                                      '_length len)
                             parent))
      (define resolved-len (resolve-length len #:parent parent))
      (cond
        [(or (not resolved-len) (eq? length-type 'bytes))
         (define end-pos (cond
                           ;; resolved-len is byte length
                           [resolved-len (+ (pos port) resolved-len)]
                           ;; no resolved-len, but parent has length
                           [(and parent (not (zero? (dict-ref parent '_length))))
                            (+ (dict-ref parent '_startOffset) (dict-ref parent '_length))]
                           ;; no resolved-len or parent, so consume whole stream
                           [else +inf.0]))
         (for/list ([i (in-naturals)]
                    #:break (or (eof-object? (peek-byte)) (= (pos port) end-pos)))
           (send type xxdecode port new-parent))]
        ;; we have resolved-len, which is treated as count of items
        [else (for/list ([i (in-range resolved-len)])
                (send type xxdecode port new-parent))]))

    (define/augment (xxencode array port [parent #f])
      (unless (sequence? array)
        (raise-argument-error 'xarray-encode "sequence" array))
      (define (encode-items parent)
        ;; todo: should array with fixed length stop encoding after it reaches max?
        ;; cf. xstring, which rejects input that is too big for fixed length.
        (let* (#;[items (sequence->list array)]
               #;[item-count (length items)]
               #;[max-items (if (number? (xarray-len xa)) (xarray-len xa) item-count)])
          (for ([item array])
            (send type xxencode item port parent))))
      (cond
        [(xint? len)
         (let ([parent (mhash 'pointers null
                              'startOffset (pos port)
                              'parent parent)])
           (dict-set! parent 'pointerOffset (+ (pos port) (xxsize array parent)))
           (send len xxencode (length array) port) ; encode length at front
           (encode-items parent)
           (for ([ptr (in-list (dict-ref parent 'pointers))]) ; encode pointer data at end
             (send (dict-ref ptr 'type) xxencode (dict-ref ptr 'val) port)))]
        [else (encode-items parent)]))

    (define/augment (xxsize [val #f] [parent #f])
      (when val (unless (sequence? val)
                  (raise-argument-error 'xarray-size "sequence" val)))
      (cond
        [val (define-values (new-parent len-size)
               (if (xint? len)
                   (values (mhasheq 'parent parent) (send len xxsize))
                   (values parent 0)))
             (define items-size (for/sum ([item val])
                                  (send type xxsize item new-parent)))
             (+ items-size len-size)]
        [else (define item-count (resolve-length len #f #:parent parent))
              (define item-size (send type xxsize #f parent))
              (* item-size item-count)]))))

(define (+xarray [type-arg #f] [len-arg #f] [length-type-arg 'count]
                 #:type [type-kwarg #f]
                 #:length [len-kwarg #f]
                 #:count-bytes [count-bytes? #f]
                 #:subclass [class xarray%])
  (new class [type (or type-arg type-kwarg)]
       [len (or len-arg len-kwarg)]
       [length-type (if count-bytes? 'bytes length-type-arg)]))
  
(module+ test
  (require rackunit "generic.rkt")
  (check-equal? (decode (+xarray uint16be 3) #"ABCDEF") '(16706 17220 17734))
  (check-equal? (encode (+xarray uint16be 3) '(16706 17220 17734) #f) #"ABCDEF")
  (check-equal? (size (+xarray uint16be) '(1 2 3)) 6)
  (check-equal? (size (+xarray doublebe) '(1 2 3 4 5)) 40))
