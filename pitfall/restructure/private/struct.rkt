#lang reader (submod "racket.rkt" reader)
(require racket/dict "stream.rkt" racket/private/generic-methods racket/struct)
(provide (all-defined-out) ref* ref*-set! (all-from-out racket/dict))
(require (prefix-in d: racket/dict))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#


(define private-keys '(parent _startOffset _currentOffset _length))

(define (choose-dict d k)
  (if (memq k private-keys)
      (get-field _pvt d)
      (get-field _kv d)))

(define dictable<%>
  (interface* ()
              ([(generic-property gen:dict)
                (generic-method-table gen:dict
                                      (define (dict-set! d k v) (d:dict-set! (choose-dict d k) k v))
                                      (define (dict-ref d k [thunk #f])
                                        (define res (d:dict-ref (choose-dict d k) k thunk))
                                        (if (LazyThunk? res) ((LazyThunk-proc res)) res))
                                      (define (dict-remove! d k) (d:dict-remove! (choose-dict d k) k))
                                      ;; public keys only
                                      (define (dict-keys d) (d:dict-keys (get-field _kv d))))]
               [(generic-property gen:custom-write)
                (generic-method-table gen:custom-write
                                      (define (write-proc o port mode)
                                        (define proc (case mode
                                                       [(#t) write]
                                                       [(#f) display]
                                                       [else (λ (p port) (print p port mode))]))
                                        (proc (get-field _kv o) port)))])))

(define StructDictRes (class* RestructureBase (dictable<%>)
                        (super-make-object)
                        (field [_kv (mhasheq)]
                               [_pvt (mhasheq)])
                        
                        (define/override (dump) _kv)))


(define-subclass Streamcoder (Struct [fields (dictify)])
  (field [[_process process] (λ (res stream ctx) res)]
         [[_preEncode preEncode] void]) ; store as field so it can be mutated from outside
  
  (define/overment (process res stream [ctx #f])
    (let* ([res (_process res stream ctx)]
           [res (inner res process res stream ctx)])
      (unless (dict? res) (raise-result-error 'Struct:process "dict" res))
      res))
  
  (define/override (preEncode . args) (apply _preEncode args))
  
  (unless ((disjoin assocs? Struct?) fields) ; should be Versioned Struct but whatever
    (raise-argument-error 'Struct "assocs or Versioned Struct" fields))

  (define/augride (decode stream [parent #f] [len 0])
    ;; _setup and _parse-fields are separate to cooperate with VersionedStruct
    (let* ([res (_setup stream parent len)]
           [res (_parse-fields stream res fields)]
           [res (process res stream)])
      res))

  (define/public-final (_setup stream parent len)
    (define res (make-object StructDictRes)) ; not mere hash
    (dict-set*! res 'parent parent
                '_startOffset (· stream pos)
                '_currentOffset 0
                '_length len)
    res)

  (define/public-final (_parse-fields stream res fields)
    (unless (assocs? fields)
      (raise-argument-error '_parse-fields "assocs" fields))
    (for/fold ([res res])
              ([(key type) (in-dict fields)])
      (define val (if (procedure? type)
                      (type res)
                      (send type decode stream res)))
      (unless (void? val)
        (ref-set! res key val))
      (ref-set! res '_currentOffset (- (· stream pos) (· res _startOffset)))
      res))
  

  (define/override (size [val #f] [parent #f] [include-pointers #t])
    (define ctx (mhasheq 'parent parent
                         'val val
                         'pointerSize 0))
    (+ (for/sum ([(key type) (in-dict fields)])
                (send type size (and val (ref val key)) ctx))
       (if include-pointers (· ctx pointerSize) 0)))

  (define/augride (encode stream val [parent #f])
    (unless (dict? val)
      (raise-argument-error 'Struct:encode "dict" val))

    (send this preEncode val stream) ; preEncode goes first, because it might bring input dict into compliance
    (define ctx (mhash 'pointers empty
                       'startOffset (· stream pos)
                       'parent parent
                       'val val
                       'pointerSize 0))
    (ref-set! ctx 'pointerOffset (+ (· stream pos) (size val ctx #f)))

    (unless (andmap (λ (key) (memq key (dict-keys val))) (dict-keys fields))
      (raise-argument-error 'Struct:encode
                            (format "dict that contains superset of Struct keys: ~a" (dict-keys fields)) (dict-keys val)))
    (for ([(key type) (in-dict fields)])
         (send type encode stream (ref val key) ctx))
    (for ([ptr (in-list (· ctx pointers))])
         (send (· ptr type) encode stream (· ptr val) (· ptr parent)))))


(test-module
 (require "number.rkt")
 (define (random-pick xs) (list-ref xs (random (length xs))))
 (check-exn exn:fail:contract? (λ () (+Struct 42)))

 ;; make random structs and make sure we can round trip
 (for ([i (in-range 20)])
      (define field-types (for/list ([i (in-range 40)])
                                    (random-pick (list uint8 uint16be uint16le uint32be uint32le double))))
      (define size-num-types (for/sum ([num-type (in-list field-types)])
                                      (send num-type size)))
      (define s (+Struct (for/list ([num-type (in-list field-types)])
                                   (cons (gensym) num-type))))
      (define bs (apply bytes (for/list ([i (in-range size-num-types)])
                                        (random 256))))
      (check-equal? (send s encode #f (send s decode bs)) bs)))
                   
 

