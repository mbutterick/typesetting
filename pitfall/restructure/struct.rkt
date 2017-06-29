#lang restructure/racket
(require racket/dict "stream.rkt" racket/private/generic-methods racket/struct)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#

(define hashable<%>
  (interface* ()
              ([(generic-property gen:indexable)
                (generic-method-table gen:indexable
                                      (define (ref o i) (or (hash-ref (get-field kv o) i #f)
                                                            (hash-ref (get-field _hash o) i #f)))
                                      (define (ref-set! o i v) (hash-set! (get-field kv o) i v))
                                      (define (ref-keys o) (hash-keys (get-field kv o))))]
               [(generic-property gen:custom-write)
                (generic-method-table gen:custom-write
                                      (define (write-proc o port mode)
                                        (define proc (case mode
                                                       [(#t) write]
                                                       [(#f) display]
                                                       [else (λ (p port) (print p port mode))]))
                                        (proc (get-field kv o) port)))])))

(define StructRes (class* RestructureBase (hashable<%>)
                    (super-make-object)
                    (field [kv (mhasheq)])
                    (define/public (ht) kv)))

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

  (define/augride (encode stream input-hash [parent #f])
    
    #;(unless (hash? input-hash)
        (raise-argument-error 'Struct:encode "hash" input-hash))

    (send this preEncode input-hash stream) ; preEncode goes first, because it might bring input hash into compliance

    (unless (andmap (λ (key) (member key (ref-keys input-hash))) (dict-keys fields))
      (raise-argument-error 'Struct:encode (format "hash that contains superset of Struct keys: ~a" (dict-keys fields)) (hash-keys input-hash)))

    (cond
      [(dict? fields)
       (for* ([(key type) (in-dict fields)])
         (send type encode stream (ref input-hash key)))]
      [else (send fields encode stream input-hash parent)]))

  (define/public-final (_setup stream parent length)
    (define res (make-object StructRes)) ; not mere hash
    (hash-set*! (· res _hash) 'parent parent
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
  

  (define/override (size [input-hash (mhash)] [parent #f] [includePointers #t])
    (for/sum ([(key type) (in-dict fields)])
      (define val (hash-ref input-hash key #f))
      (define args (if val (list val) empty))
      (send type size . args))))


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
                   
 

