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

(define x:array%
  (class x:enobase%
    (super-new)
    (init-field [(@type type)] [(@len len)] [(@length-type length-type)])
    
    (unless (xenomorphic-type? @type)
      (raise-argument-error '+xarray "xenomorphic type" @type))
    (unless (length-resolvable? @len)
      (raise-argument-error '+xarray "length-resolvable?" @len))
    (unless (memq @length-type '(bytes count))
      (raise-argument-error '+xarray "'bytes or 'count" @length-type))

    (define/augride (x:decode port parent)
      (define new-parent (if (xint? @len)
                             (mhasheq 'parent parent
                                      '_startOffset (pos port)
                                      '_currentOffset 0
                                      '_length @len)
                             parent))
      (define len (resolve-length @len port #:parent parent))
      (cond
        [(or (not len) (eq? @length-type 'bytes))
         (define end-pos (cond
                           ;; resolved-len is byte length
                           [len (+ (pos port) len)]
                           ;; no resolved-len, but parent has length
                           [(and parent (not (zero? (dict-ref parent '_length))))
                            (+ (dict-ref parent '_startOffset) (dict-ref parent '_length))]
                           ;; no resolved-len or parent, so consume whole stream
                           [else +inf.0]))
         (for/list ([i (in-naturals)]
                    #:break (or (eof-object? (peek-byte port)) (= (pos port) end-pos)))
           (send @type x:decode port new-parent))]
        ;; we have resolved-len, which is treated as count of items
        [else (for/list ([i (in-range len)])
                (send @type x:decode port new-parent))]))

    (define/augride (x:encode array port [parent #f])
      (unless (sequence? array)
        (raise-argument-error 'xarray-encode "sequence" array))
      (define (encode-items parent)
        ;; todo: should array with fixed length stop encoding after it reaches max?
        ;; cf. xstring, which rejects input that is too big for fixed length.
        (let* (#;[items (sequence->list array)]
               #;[item-count (length items)]
               #;[max-items (if (number? (xarray-len xa)) (xarray-len xa) item-count)])
          (for ([item array])
            (send @type x:encode item port parent))))
      (cond
        [(xint? @len)
         (define new-parent (mhash 'pointers null
                                   'startOffset (pos port)
                                   'parent parent))
         (dict-set! new-parent 'pointerOffset (+ (pos port) (x:size array new-parent)))
         (send @len x:encode (length array) port) ; encode length at front
         (encode-items new-parent)
         (for ([ptr (in-list (dict-ref new-parent 'pointers))]) ; encode pointer data at end
           (send (dict-ref ptr 'type) x:encode (dict-ref ptr 'val) port))]
        [else (encode-items parent)]))

    (define/augride (x:size [val #f] [parent #f])
      (when val (unless (sequence? val)
                  (raise-argument-error 'xarray-size "sequence" val)))
      (cond
        [val (define-values (new-parent len-size)
               (if (xint? @len)
                   (values (mhasheq 'parent parent) (send @len x:size))
                   (values parent 0)))
             (define items-size (for/sum ([item val])
                                  (send @type x:size item new-parent)))
             (+ items-size len-size)]
        [else (define count (resolve-length @len #f #:parent parent))
              (define size (send @type x:size #f parent))
              (* size count)]))))

(define (x:array [type-arg #f] [len-arg #f] [length-type-arg 'count]
                 #:type [type-kwarg #f]
                 #:length [len-kwarg #f]
                 #:count-bytes [count-bytes? #f]
                 #:pre-encode [pre-proc #f]
                 #:post-decode [post-proc #f])
  (new (generate-subclass x:array% pre-proc post-proc) [type (or type-arg type-kwarg)]
       [len (or len-arg len-kwarg)]
       [length-type (if count-bytes? 'bytes length-type-arg)]))

(define (xarray? x) (is-a? x x:array%))
  
(module+ test
  (require rackunit "generic.rkt")
  (check-equal? (decode (x:array uint16be 3) #"ABCDEF") '(16706 17220 17734))
  (check-equal? (encode (x:array uint16be 3) '(16706 17220 17734) #f) #"ABCDEF")
  (check-equal? (size (x:array uint16be) '(1 2 3)) 6)
  (check-equal? (size (x:array doublebe) '(1 2 3 4 5)) 40))
