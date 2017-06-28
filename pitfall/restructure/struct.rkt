#lang restructure/racket
(require racket/dict "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#

(define-subclass Streamcoder (Struct [fields (dictify)])
  
  (unless ((disjoin assocs? Struct?) fields) ; should be Versioned Struct but whatever
    (raise-argument-error 'Struct "assocs or Versioned Struct" fields))

  (define/augride (decode stream [parent #f] [length_ 0])
    (define res (_setup stream parent length_))
    (_parseFields stream res fields)
    (send this process res stream)
    res)

  (define/augride (encode stream input-hash [parent #f])
    
    (unless (hash? input-hash)
      (raise-argument-error 'Struct:encode "hash" input-hash))

    (send this preEncode input-hash stream) ; preEncode goes first, because it might bring input hash into compliance

    (unless (andmap (λ (key) (member key (hash-keys input-hash))) (dict-keys fields))
      (raise-argument-error 'Struct:encode (format "hash that contains superset of Struct keys: ~a" (dict-keys fields)) (hash-keys input-hash)))

    (cond
      [(dict? fields)
       (for* ([(key type) (in-dict fields)])
         (send type encode stream (hash-ref input-hash key)))]
      [else (send fields encode stream input-hash parent)]))

  (define/public-final (_setup stream parent length)
    (define res (mhasheq))
    (hash-set*! res 'parent parent
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
            (send type decode stream this)))
      (hash-set! res key val)
      (hash-set! res '_currentOffset (- (· stream pos) (· res _startOffset)))))

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
                   
 

