#lang debug racket/base
(require (prefix-in d: racket/dict)
         racket/promise
         racket/sequence
         racket/list
         "helper.rkt"
         "number.rkt"
         sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#


(define (choose-dict d k)
  (if (memq k private-keys)
      (struct-dict-res-_pvt d)
      (struct-dict-res-_kv d)))

(struct struct-dict-res (_kv _pvt) #:transparent
  #:methods d:gen:dict
  [(define (dict-set! d k v) (d:dict-set! (choose-dict d k) k v))
   (define (dict-ref d k [thunk #f])
     (define res (d:dict-ref (choose-dict d k) k thunk))
     (force res))
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
                    (xdecode type port #:parent sdr)))
    (unless (void? val)
      (d:dict-set! sdr key val))
    (d:dict-set! sdr '_currentOffset (- (pos port) (d:dict-ref sdr '_startOffset)))
    sdr))

(define (xstruct-decode . args)
  (dict->mutable-hash (apply xstruct-xdecode args)))

(define (xstruct-xdecode xs [port-arg (current-input-port)] #:parent [parent #f] [len 0])
  (define port (->input-port port-arg))
  (parameterize ([current-input-port port])
    ;; _setup and _parse-fields are separate to cooperate with VersionedStruct
    (define res
      (post-decode xs
                   (let* ([sdr (_setup port parent len)] ; returns StructDictRes
                          [sdr (_parse-fields port sdr (xstruct-fields xs))])
                     sdr)))
    (unless (d:dict? res)
      (raise-result-error 'xstruct-decode "dict" res))
    res))

(define/finalize-size (xstruct-size xs [val #f] #:parent [parent-arg #f] [include-pointers #t])
  (define parent (mhasheq 'parent parent-arg
                          'val val
                          'pointerSize 0))
  (define fields-size (for/sum ([(key type) (d:in-dict (xstruct-fields xs))]
                                #:when (xenomorphic? type))
                               (size type (and val (d:dict-ref val key)) #:parent parent)))
  (define pointers-size (if include-pointers (d:dict-ref parent 'pointerSize) 0))
  (+ fields-size pointers-size))

(define (xstruct-encode xs val-arg [port-arg (current-output-port)] #:parent [parent-arg #f])
  (unless (d:dict? val-arg)
    (raise-argument-error 'xstruct-encode "dict" val-arg))
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (parameterize ([current-output-port port])
    ;; check keys first, since `size` also relies on keys being valid
    (define val (let* ([val (pre-encode xs val-arg)]
                       #;[val (inner res pre-encode val . args)])
                  (unless (d:dict? val) (raise-result-error 'xstruct-encode "dict" val))
                  val))
    (unless (andmap (λ (key) (memq key (d:dict-keys val))) (d:dict-keys (xstruct-fields xs)))
      (raise-argument-error 'xstruct-encode
                            (format "dict that contains superset of Struct keys: ~a" (d:dict-keys (xstruct-fields xs))) (d:dict-keys val)))

    (define parent (mhash 'pointers empty
                          'startOffset (pos port)
                          'parent parent-arg
                          'val val
                          'pointerSize 0))

    ; deliberately use `xstruct-size` instead of `size` to use extra arg
    (d:dict-set! parent 'pointerOffset (+ (pos port) (xstruct-size xs val #:parent parent #f))) 

    (for ([(key type) (d:in-dict (xstruct-fields xs))])
         (encode type (d:dict-ref val key) #:parent parent))
    (for ([ptr (in-list (d:dict-ref parent 'pointers))])
         (encode (d:dict-ref ptr 'type) (d:dict-ref ptr 'val) #:parent (d:dict-ref ptr 'parent)))
    (unless port-arg (get-output-bytes port))))

(struct structish xbase () #:transparent)
(struct xstruct structish (fields) #:transparent #:mutable
  #:methods gen:xenomorphic
  [(define decode xstruct-decode)
   (define xdecode xstruct-xdecode)
   (define encode xstruct-encode)
   (define size xstruct-size)])

(define (+xstruct . dicts)
  (define args (flatten dicts))
  (unless (even? (length args))
    (raise-argument-error '+xstruct "equal keys and values" dicts))
  (define fields (for/list ([kv (in-slice 2 args)])
                           (unless (symbol? (car kv))
                             (raise-argument-error '+xstruct "symbol" (car kv)))
                           (apply cons kv)))
  (unless (d:dict? fields)
    (raise-argument-error '+xstruct "dict" fields))
  (xstruct fields))

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