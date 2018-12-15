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


(define (xstruct-setup port parent len)
  (define mheq (make-hasheq))
  (d:dict-set*! mheq
                'parent parent
                '_startOffset (pos port)
                '_currentOffset 0
                '_length len)
  mheq)

(define (xstruct-parse-fields port sdr fields-arg)
  (define fields (if (xstruct? fields-arg) (xstruct-fields fields-arg) fields-arg))
  (unless (assocs? fields)
    (raise-argument-error 'xstruct-parse-fields "assocs" fields))
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
    ;; xstruct-setup and xstruct-parse-fields are separate to cooperate with VersionedStruct
    (define decoded-hash
      (post-decode xs
                   (let* ([mheq (xstruct-setup port parent len)] ; returns StructDictRes
                          [mheq (xstruct-parse-fields port mheq (xstruct-fields xs))])
                     mheq)))
    (unless (d:dict? decoded-hash)
      (raise-result-error 'xstruct-decode "dict" decoded-hash))
    decoded-hash))

(define/finalize-size (xstruct-size xs [val #f] #:parent [parent-arg #f] #:include-pointers [include-pointers #t])
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
    (define val (let* ([val (pre-encode xs val-arg)])
                  (unless (d:dict? val)
                    (raise-result-error 'xstruct-encode "dict" val))
                  val))
    (unless (andmap (λ (key) (memq key (d:dict-keys val))) (d:dict-keys (xstruct-fields xs)))
      (raise-argument-error 'xstruct-encode
                            (format "dict that contains superset of xstruct keys: ~a" (d:dict-keys (xstruct-fields xs))) (d:dict-keys val)))

    (define parent (mhash 'pointers empty
                          'startOffset (pos port)
                          'parent parent-arg
                          'val val
                          'pointerSize 0))

    ; deliberately use `xstruct-size` instead of `size` to use extra arg
    (d:dict-set! parent 'pointerOffset
                 (+ (pos port) (xstruct-size xs val #:parent parent #:include-pointers #f)))

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
    (raise-argument-error '+xstruct "equal number of keys and values" dicts))
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