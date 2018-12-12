#lang debug racket/base
(require (prefix-in d: racket/dict) racket/list "helper.rkt" "util.rkt" "number.rkt" sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#

(define private-keys '(parent _startOffset _currentOffset _length))

(define (choose-dict d k)
  (if (memq k private-keys)
      (struct-dict-res-_pvt d)
      (struct-dict-res-_kv d)))

(struct struct-dict-res (_kv _pvt) #:transparent
  #:methods d:gen:dict
  [(define (dict-set! d k v) (d:dict-set! (choose-dict d k) k v))
   (define (dict-ref d k [thunk #f])
     (define res (d:dict-ref (choose-dict d k) k thunk))
     (if (lazy-thunk? res) ((lazy-thunk-proc res)) res))
   (define (dict-remove! d k) (d:dict-remove! (choose-dict d k) k))
   ;; public keys only
   (define (dict-keys d) (d:dict-keys (struct-dict-res-_kv d)))
   (define (dict-iterate-first d) (and (pair? (dict-keys d)) 0))
   (define (dict-iterate-next d i) (and (< (add1 i) (length (dict-keys d))) (add1 i)))
   (define (dict-iterate-key d i) (list-ref (dict-keys d) i))
   (define (dict-iterate-value d i) (dict-ref d (dict-iterate-key d i)))])

(define (+struct-dict-res [_kv (mhasheq)] [_pvt (mhasheq)])
  (struct-dict-res _kv _pvt))

(define (_setup port parent len)
  (define sdr (+struct-dict-res)) ; not mere hash
  (d:dict-set*! sdr 'parent parent
                '_startOffset (pos port)
                '_currentOffset 0
                '_length len)
  sdr)

(define (_parse-fields port sdr fields)
  (unless (assocs? fields)
    (raise-argument-error '_parse-fields "assocs" fields))
  (for/fold ([sdr sdr])
            ([(key type) (d:in-dict fields)])
    (define val (if (procedure? type)
                    (type sdr)
                    (decode type port #:parent sdr)))
    (unless (void? val)
      (d:dict-set! sdr key val))
    (d:dict-set! sdr '_currentOffset (- (pos port) (d:dict-ref sdr '_startOffset)))
    sdr))

(define (xstruct-decode xs [port-arg (current-input-port)] #:parent [parent #f] [len 0])
  (define port (->input-port port-arg))
  (parameterize ([current-input-port port])
    ;; _setup and _parse-fields are separate to cooperate with VersionedStruct
    (define res
      (let* ([sdr (_setup port parent len)] ; returns StructDictRes
             [sdr (_parse-fields port sdr (xstruct-fields xs))])
        sdr))
    (let* ([res ((xstruct-post-decode xs) res port parent)]
           #;[res (inner res post-decode res . args)])
      (unless (d:dict? res) (raise-result-error 'xstruct-decode "dict" res))
      res)))

(define (xstruct-size xs [val #f] [parent #f] [include-pointers #t])
  (define ctx (mhasheq 'parent parent
                       'val val
                       'pointerSize 0))
  (+ (for/sum ([(key type) (d:in-dict (xstruct-fields xs))]
               #:when (xenomorphic? type))
       (size type (and val (d:dict-ref val key)) ctx))
     (if include-pointers (d:dict-ref ctx 'pointerSize) 0)))

(define (xstruct-encode xs val-arg [port-arg (current-output-port)] #:parent [parent #f])
  (unless (d:dict? val-arg)
    (raise-argument-error 'xstruct-encode "dict" val-arg))
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (parameterize ([current-output-port port])
    ;; check keys first, since `size` also relies on keys being valid
    (define val (let* ([val ((xstruct-pre-encode xs) val-arg port)]
                       #;[val (inner res pre-encode val . args)])
                  (unless (d:dict? val) (raise-result-error 'xstruct-encode "dict" val))
                  val))
    (unless (andmap (λ (key) (memq key (d:dict-keys val))) (d:dict-keys (xstruct-fields xs)))
      (raise-argument-error 'xstruct-encode
                            (format "dict that contains superset of Struct keys: ~a" (d:dict-keys (xstruct-fields xs))) (d:dict-keys val)))

    (define ctx (mhash 'pointers empty
                       'startOffset (pos port)
                       'parent parent
                       'val val
                       'pointerSize 0))

    ; deliberately use `xstruct-size` instead of `size` to use extra arg
    (d:dict-set! ctx 'pointerOffset (+ (pos port) (xstruct-size xs val ctx #f))) 

    (for ([(key type) (d:in-dict (xstruct-fields xs))])
      (encode type (d:dict-ref val key) #:parent ctx))
    (for ([ptr (in-list (d:dict-ref ctx 'pointers))])
      (encode (d:dict-ref ptr 'type) (d:dict-ref ptr 'val) #:parent (d:dict-ref ptr 'parent)))
    (unless port-arg (get-output-bytes port))))

(struct structish () #:transparent)
(struct xstruct structish (fields post-decode pre-encode) #:transparent #:mutable
  #:methods gen:xenomorphic
  [(define decode xstruct-decode)
   (define encode xstruct-encode)
   (define size xstruct-size)])

(define (+xstruct [fields null] [post-decode (λ (val port ctx) val)] [pre-encode (λ (val port) val)])
  (unless (d:dict? fields)
    (raise-argument-error '+xstruct "dict" fields))
  (xstruct fields post-decode pre-encode))

(module+ test
  (require rackunit "number.rkt")
  (define (random-pick xs) (list-ref xs (random (length xs))))
  (check-exn exn:fail:contract? (λ () (+xstruct 42)))
  (for ([i (in-range 20)])
    ;; make random structs and make sure we can round trip
    (define field-types
      (for/list ([i (in-range 40)])
        (random-pick (list uint8 uint16be uint16le uint32be uint32le double))))
    (define size-num-types
      (for/sum ([num-type (in-list field-types)])
        (size num-type)))
    (define xs (+xstruct (for/list ([num-type (in-list field-types)])
                           (cons (gensym) num-type))))
    (define bs (apply bytes (for/list ([i (in-range size-num-types)])
                              (random 256))))
    (check-equal? (encode xs (decode xs bs) #f) bs)))