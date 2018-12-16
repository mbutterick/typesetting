#lang debug racket/base
(require racket/dict
         racket/class
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
  (dict-set*! mheq
              'parent parent
              '_startOffset (pos port)
              '_currentOffset 0
              '_length len)
  mheq)

(define (xstruct-parse-fields port sdr fields-arg)
  (define fields (if (xstruct? fields-arg) (get-field fields fields-arg) fields-arg))
  (unless (assocs? fields)
    (raise-argument-error 'xstruct-parse-fields "assocs" fields))
  (for/fold ([sdr sdr])
            ([(key type) (in-dict fields)])
    (define val (if (procedure? type)
                    (type sdr)
                    (send type xxdecode port sdr)))
    (unless (void? val)
      (dict-set! sdr key val))
    (dict-set! sdr '_currentOffset (- (pos port) (dict-ref sdr '_startOffset)))
    sdr))

(define private-keys '(parent _startOffset _currentOffset _length))

(define (dict->mutable-hash x)
  (define h (make-hasheq))
  (for ([(k v) (in-dict x)]
        #:unless (memq k private-keys))
       (hash-set! h k v))
  h)

(define xstruct%
  (class xenobase%
    (super-new)
    (init-field [(@fields fields)])
    
    (when @fields (unless (dict? @fields)
                    (raise-argument-error '+xstruct "dict" @fields)))

    (define/augride (xxdecode port parent [len 0])
      ;; xstruct-setup and xstruct-parse-fields are separate to cooperate with VersionedStruct
      (define decoded-hash
        (xstruct-parse-fields port (xstruct-setup port parent len) @fields))
      (unless (dict? decoded-hash)
        (raise-result-error 'xstruct-decode "dict" decoded-hash))
      decoded-hash)

    (define/override (decode port parent)
      (dict->mutable-hash (xxdecode port parent)))

    (define/augride (xxencode val port [parent-arg #f])
      ;; check keys first, since `size` also relies on keys being valid
      (unless (dict? val)
        (raise-result-error 'xstruct-encode "dict" val))
      (unless (andmap (λ (key) (memq key (dict-keys val))) (dict-keys @fields))
        (raise-argument-error 'xstruct-encode
                              (format "dict that contains superset of xstruct keys: ~a"
                                      (dict-keys @fields)) (dict-keys val))) 
      (define parent (mhash 'pointers empty
                            'startOffset (pos port)
                            'parent parent-arg
                            'val val
                            'pointerSize 0)) 
      (dict-set! parent 'pointerOffset (+ (pos port) (xxsize val parent #f))) 
      (for ([(key type) (in-dict @fields)])
           (send type xxencode (dict-ref val key) port parent))
      (for ([ptr (in-list (dict-ref parent 'pointers))])
           (send (dict-ref ptr 'type) xxencode (dict-ref ptr 'val) port (dict-ref ptr 'parent))))
    
    (define/augride (xxsize [val #f] [parent-arg #f] [include-pointers #t])
      (define parent (mhasheq 'parent parent-arg
                              'val val
                              'pointerSize 0))
      (define fields-size (for/sum ([(key type) (in-dict @fields)]
                                    #:when (xenomorphic-type? type))
                                   (send type xxsize (and val (dict-ref val key)) parent)))
      (define pointers-size (if include-pointers (dict-ref parent 'pointerSize) 0))
      (+ fields-size pointers-size))))

(define (xstruct? x) (is-a? x xstruct%))

(define (+xstruct #:subclass [class xstruct%] . dicts)
  (define args (flatten dicts))
  (unless (even? (length args))
    (raise-argument-error '+xstruct "equal number of keys and values" dicts))
  (define fields (for/list ([kv (in-slice 2 args)])
                           (unless (symbol? (car kv))
                             (raise-argument-error '+xstruct "symbol" (car kv)))
                           (apply cons kv)))
  (new class [fields fields]))

(module+ test
  (require rackunit "number.rkt" "generic.rkt")
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