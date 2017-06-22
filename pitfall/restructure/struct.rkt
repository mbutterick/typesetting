#lang restructure/racket
(require racket/dict "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#

(define-subclass Streamcoder (Struct [assocs (dictify)])
  (field [res #f])
  
  (unless (assocs? assocs)
    (raise-argument-error 'Struct "assocs" assocs))
  (field [key-index #f] ; store the original key order
         [fields (mhash)])

  (define/private (update-key-index! assocs)
    (unless (assocs? assocs)
      (raise-argument-error 'Struct "assocs" assocs))
    (set! key-index (map car assocs)))

  (update-key-index! assocs)
  
  (define/public-final (update-fields! assocs)
    (unless (assocs? assocs)
      (raise-argument-error 'Struct "assocs or hash" assocs))
    (update-key-index! assocs)
    (for ([(k v) (in-dict assocs)])
         (hash-set! fields k v)))
  
  (update-fields! assocs)
  
  (define/augride (decode stream [parent #f] [length 0])
    (set! res (_setup stream parent length))
    (_parseFields stream fields)
    (send this process res stream)
    res)

  (define/augment (encode stream input-hash [parent #f])
    
    (unless (hash? input-hash)
      (raise-argument-error 'Struct:encode "hash" input-hash))

    (send this preEncode input-hash stream) ; preEncode goes first, because it might bring input hash into compliance

    (inner (void) encode stream input-hash parent)

    (unless (andmap (λ (key) (member key (hash-keys input-hash))) key-index)
      (raise-argument-error 'Struct:encode (format "hash that contains superset of Struct keys: ~a" key-index) (hash-keys input-hash)))
    
    (for* ([key (in-list key-index)] ; iterate over original keys in order
           [struct-type (in-value (hash-ref fields key))]
           [value-to-encode (in-value (hash-ref input-hash key))])
          (send struct-type encode stream value-to-encode)))

  (define/public-final (_setup stream parent length)
    (mhasheq))

  (define/public-final (_parseFields stream fields)
    (for ([key (in-list key-index)])         
         (define dictvalue (dict-ref fields key))
         (define val
           (if (procedure? dictvalue)
               (dictvalue res)
               (send dictvalue decode stream this)))
         (hash-set! res key val)))

  (define/overment (size [input-hash (mhash)] [parent #f] [includePointers #t])
    (inner (void) size input-hash parent includePointers)
    (for/sum ([(key type) (in-hash fields)])
             (define val (hash-ref input-hash key #f))
             (define args (if val (list val) empty))
             (send type size . args))))


(test-module
 (require "number.rkt")
 (define (random-pick xs) (list-ref xs (random (length xs))))
 (check-exn exn:fail:contract? (λ () (+Struct 42)))

 ;; make random structs and make sure we can round trip
 (for ([i (in-range 100)])
      (define field-types (for/list ([i (in-range 200)])
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
                   
 


#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/VersionedStruct.coffee
|#

(define-subclass Struct (VersionedStruct version-resolver [versions (dictify)])
  (inherit-field res)
  (unless ((disjoin integer? procedure? RestructureBase? symbol?) version-resolver)
    (raise-argument-error 'VersionedStruct "integer, function, symbol, or Restructure object" version-resolver))
  (unless (and (dict? versions) (andmap (λ (val) (or (dict? val) (Struct? val))) (map cdr versions)))
    (raise-argument-error 'VersionedStruct "dict of dicts or Structs" versions))
  (inherit-field fields key-index)
  (field [forced-version #f])
  
  (define/public-final (force-version! version)
    (set! forced-version version))

  (define/public (resolve-version [stream #f] [parent #f])
    (cond
      [forced-version] ; for testing purposes: pass an explicit version
      [(integer? version-resolver) version-resolver]
      [(symbol? version-resolver) (hash-ref (· parent res) version-resolver)]
      [(and (procedure? version-resolver) (positive? (procedure-arity version-resolver))) (version-resolver parent)]
      [(RestructureBase? version-resolver) (send version-resolver decode stream)]
      [else (raise-argument-error 'VersionedStruct:resolve-version "way of finding version" version-resolver)]))
  
  (define/override (decode stream [parent #f] [length 0])
    (set! res (send this _setup stream parent length))
    (define version (resolve-version stream parent))
    (hash-set! res 'version version)
    (define assocs (dict-ref versions version (λ () (raise-argument-error 'VersionedStruct:decode "valid version key" version))))
    (send this update-fields! assocs)
    (cond
      [(VersionedStruct? fields) (send fields decode stream parent)]
      [else
       (send this _parseFields stream fields)
       (send this process res stream)
       res]))

  (define/augment (encode stream input-hash [parent #f])
    (define assocs (dict-ref versions (· input-hash version) (λ () (raise-argument-error 'VersionedStruct:encode "valid version key" version))))
    (send this update-fields! assocs))
  

  (define/augment (size [val (mhash)] [parent #f] [includePointers #t])
    (define version (resolve-version #f parent))
    (define assocs (dict-ref versions version (λ () (raise-argument-error 'VersionedStruct:size "valid version key" version))))
    (send this update-fields! assocs)))

(test-module
 (require "number.rkt")
 (check-exn exn:fail:contract? (λ () (+VersionedStruct 42 42)))

 ;; make random versioned structs and make sure we can round trip
 (for ([i (in-range 20)])
      (define field-types (for/list ([i (in-range 200)])
                                    (random-pick (list uint8 uint16be uint16le uint32be uint32le double))))
      (define num-versions 20)
      (define which-struct (random num-versions))
      (define struct-versions (for/list ([v (in-range num-versions)])
                                        (cons v (for/list ([num-type (in-list field-types)])
                                                          (cons (gensym) num-type)))))
      (define vs (+VersionedStruct which-struct struct-versions))
      (define struct-size (for/sum ([num-type (in-list (map cdr (dict-ref struct-versions which-struct)))])
                                   (send num-type size)))
      (define bs (apply bytes (for/list ([i (in-range struct-size)])
                                        (random 256))))
      (check-equal? (send vs encode #f (send vs decode bs)) bs))

 (define s (+Struct (dictify 'a uint8 'b uint8 'c uint8)))
 (check-equal? (send s size) 3)
 (define vs (+VersionedStruct (λ (p) 2) (dictify 1 (dictify 'd s) 2 (dictify 'e s 'f s))))
 (check-equal? (send vs size) 6))


