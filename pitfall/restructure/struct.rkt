#lang restructure/racket
(require racket/dict "stream.rkt" racket/private/generic-methods racket/struct)
(provide (all-defined-out))
(require (prefix-in d: racket/dict))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#


(define private-keys '(parent _startOffset _currentOffset _length))

(define dictable<%>
  (interface* ()
              ([(generic-property gen:dict)
                (generic-method-table gen:dict
                                      (define (dict-set! d k v) (d:dict-set! (if (memq k private-keys)
                                                                                 (get-field pvt d)
                                                                                 (get-field kv d)) k v))
                                      (define (dict-ref d k [thunk #f]) (d:dict-ref (if (memq k private-keys)
                                                                                        (get-field pvt d)
                                                                                        (get-field kv d)) k thunk))
                                      ;; public keys only
                                      (define (dict-keys d) (d:dict-keys (get-field kv d))))])))

(define StructDictRes (class* RestructureBase (dictable<%>)
                        (super-make-object)
                        (field [kv (mhasheq)]
                               [pvt (mhasheq)])
                        (public [_kv kv])
                        (define (_kv) kv)))


(define-subclass Streamcoder (Struct [fields (dictify)])
  (field [[_process process] void]
         [[_preEncode preEncode] void]) ; store as field so it can be mutated from outside
  (define/override (process . args) (apply _process args))
  (define/override (preEncode . args) (apply _preEncode args))
  
  (unless ((disjoin assocs? Struct?) fields) ; should be Versioned Struct but whatever
    (raise-argument-error 'Struct "assocs or Versioned Struct" fields))

  (define/augride (decode stream [parent #f] [length_ 0])
    (define res (_setup stream parent length_))
    (_parseFields stream res fields)
    (process res stream)
    res)

  (define/public-final (_setup stream parent length)
    (define res (make-object StructDictRes)) ; not mere hash
    (dict-set*! res 'parent parent
                '_startOffset (· stream pos)
                '_currentOffset 0
                '_length length)
    res)

  (define/public-final (_parseFields stream res fields)
    (unless (assocs? fields)
      (raise-argument-error '_parseFields "assocs" fields))
    (for ([(key type) (in-dict fields)])
      (define val
        (if (procedure? type)
            (type res)
            (send type decode stream res)))
      ;; skip PropertyDescriptor maneuver. Only used for lazy pointer
      (ref-set! res key val)
      (hash-set! (· res _hash) '_currentOffset (- (· stream pos) (ref res '_startOffset)))))
  

  (define/override (size [val (mhash)] [parent #f] [includePointers #t])
    (define ctx (mhash 'parent parent
                       'val val
                       'pointerSize 0))
    (define size 0)
    (for ([(key type) (in-dict fields)])
      (increment! size (send type size (ref val key) ctx)))

    (when includePointers
      (increment! size (ref ctx 'pointerSize)))
    
    size)

  (define/augride (encode stream val [parent #f])
    
    #;(unless (hash? input-hash)
        (raise-argument-error 'Struct:encode "hash" input-hash))

    (send this preEncode val stream) ; preEncode goes first, because it might bring input hash into compliance

    (define ctx (mhash 'pointers empty
                       'startOffset (· stream pos)
                       'parent parent
                       'val val
                       'pointerSize 0))

    (ref-set! ctx 'pointerOffset (+ (· stream pos) (size val ctx #f)))

    (unless (andmap (λ (key) (member key (ref-keys val))) (dict-keys fields))
      (raise-argument-error 'Struct:encode (format "hash that contains superset of Struct keys: ~a" (dict-keys fields)) (hash-keys val)))

    (for ([(key type) (in-dict fields)])
      (send type encode stream (ref val key) ctx))

    (for ([ptr (in-list (ref ctx 'pointers))])
      (send (· ptr type) encode stream (· ptr val) (· ptr parent)))))


(test-module
 (require "number.rkt")
 (define (random-pick xs) (list-ref xs (random (length xs))))
 (check-exn exn:fail:contract? (λ () (+Struct 42)))

 ;; make random structs and make sure we can round trip
 (for ([i (in-range 10)])
   (define field-types (for/list ([i (in-range 20)])
                         (random-pick (list uint8 uint16be uint16le uint32be uint32le double))))
   (define size-num-types (for/sum ([num-type (in-list field-types)])
                            (send num-type size)))
   (define s (+Struct (for/list ([num-type (in-list field-types)])
                        (cons (gensym) num-type))))
   (define bs (apply bytes (for/list ([i (in-range size-num-types)])
                             (random 256))))
   (define es (+EncodeStream))
   (send s encode es (send s decode bs))
   (check-equal? (send es dump) bs)))
                   
 

