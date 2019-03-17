#lang debug racket/base
(require racket/class
         "base.rkt"
         "int.rkt"
         "util.rkt"
         sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Array.coffee
|#

(define x:list%
  (class x:base%
    (super-new)
    (init-field [(@type type)] [(@len len)] [(@count-bytes? count-bytes?)])
    
    (unless (xenomorphic-type? @type)
      (raise-argument-error 'x:array "xenomorphic type" @type))
    (unless (length-resolvable? @len)
      (raise-argument-error 'x:array "length-resolvable?" @len))
    (unless (boolean? @count-bytes?)
      (raise-argument-error 'x:array "boolean" @count-bytes?))

    (define/augride (x:decode port parent)
      (define new-parent (if (x:int? @len)
                             (mhasheq x:parent-key parent
                                      x:start-offset-key (pos port)
                                      x:current-offset-key 0
                                      x:length-key @len)
                             parent))
      (define len (resolve-length @len port parent))
      (cond
        [(or (not len) @count-bytes?)
         (define end-pos (cond
                           ;; len is byte length
                           [len (+ (pos port) len)]
                           ;; no len, but parent has length
                           [(and parent (not (zero? (hash-ref parent x:length-key))))
                            (+ (hash-ref parent x:start-offset-key) (hash-ref parent x:length-key))]
                           ;; no len or parent, so consume whole stream
                           [else +inf.0]))
         (for/list ([i (in-naturals)]
                    #:break (or (eof-object? (peek-byte port)) (= (pos port) end-pos)))
           (send @type x:decode port new-parent))]
        ;; we have len, which is treated as count of items
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
          (for ([item (in-list array)])
            (send @type x:encode item port parent))))
      (cond
        [(x:int? @len)
         (define new-parent (mhasheq x:pointers-key null
                                     x:start-offset-key (pos port)
                                     x:parent-key parent))
         (hash-set! new-parent x:pointer-offset-key (+ (pos port) (x:size array new-parent)))
         (send @len x:encode (length array) port) ; encode length at front
         (encode-items new-parent)
         (for ([ptr (in-list (hash-ref new-parent x:pointers-key))]) ; encode pointer data at end
           (send (x:ptr-type ptr) x:encode (x:ptr-val ptr) port))]
        [else (encode-items parent)]))

    (define/augride (x:size [val #f] [parent #f])
      (when val (unless (sequence? val)
                  (raise-argument-error 'xarray-size "sequence" val)))
      (cond
        [val (define-values (new-parent len-size)
               (if (x:int? @len)
                   (values (mhasheq x:parent-key parent) (send @len x:size))
                   (values parent 0)))
             (define items-size (for/sum ([item val])
                                  (send @type x:size item new-parent)))
             (+ items-size len-size)]
        [else (define count (resolve-length @len #f parent))
              (define size (send @type x:size #f parent))
              (* size count)]))))

(define (x:list [type-arg #f] [len-arg #f] [length-type-arg 'count]
                 #:type [type-kwarg #f]
                 #:length [len-kwarg #f]
                 #:count-bytes [count-bytes? #f]
                 #:pre-encode [pre-proc #f]
                 #:post-decode [post-proc #f]
                 #:base-class [base-class x:list%])
  (new (generate-subclass base-class pre-proc post-proc) [type (or type-arg type-kwarg)]
       [len (or len-arg len-kwarg)]
       [count-bytes? count-bytes?]))

(define (x:list? x) (is-a? x x:list%))

(define x:array% x:list%)
(define x:array x:list)
(define x:array? x:list?)
  
(module+ test
  (require rackunit "base.rkt")
  (check-equal? (decode (x:list uint16be 3) #"ABCDEF") '(16706 17220 17734))
  (check-equal? (encode (x:list uint16be 3) '(16706 17220 17734) #f) #"ABCDEF")
  (check-equal? (size (x:list uint16be) '(1 2 3)) 6))
