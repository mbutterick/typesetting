#lang restructure/racket
(require racket/dict "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#

(define-subclass Streamcoder (Struct [assocs (dictify)])
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
    (define res (_setup stream parent length))
    (_parseFields stream res fields)
    #;(hash-set! (hash-ref res '_props) '_currentOffset (· stream pos))
    (send this process res stream)
    res)

  (define/augride (encode stream input-hash [parent #f])
    (unless (hash? input-hash)
      (raise-argument-error 'Struct:encode "hash" input-hash))

    (send this preEncode input-hash stream) ; might bring input hash into compliance

    (unless (andmap (λ (key) (member key (hash-keys input-hash))) key-index)
      (raise-argument-error 'Struct:encode (format "hash that contains superset of Struct keys: ~a" key-index) (hash-keys input-hash)))
    
    (for* ([key (in-list key-index)] ; iterate over original keys in order
           [struct-type (in-value (hash-ref fields key))]
           [value-to-encode (in-value (hash-ref input-hash key))])
      (send struct-type encode stream value-to-encode)))

  (define/public-final (_setup stream parent length)
    (define res (mhasheq))

    ;; define hidden properties
    #;(hash-set! res '_props
                 (mhasheq 'parent (mhasheq 'value parent)
                          '_startOffset (mhasheq 'value (· stream pos))
                          '_currentOffset (mhasheq 'value 0 'writable #t)
                          '_length (mhasheq 'value length)))
    res)

  (define/public-final (_parseFields stream res fields)
    (for ([key (in-list key-index)])         
      (define dictvalue (dict-ref fields key))
      (define val
        (if (procedure? dictvalue)
            (dictvalue res)
            (send dictvalue decode stream res)))
      (hash-set! res key val)))

  (define/override (size [val (mhash)] [parent #f] [includePointers #t])
    (for/sum ([(key type) (in-hash fields)]
              #:when (hash-has-key? val key))
      (send type size (hash-ref val key)))))


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
  (unless ((disjoin integer? procedure? RestructureBase?) version-resolver)
    (raise-argument-error 'VersionedStruct "integer, function, or Restructure object" version-resolver))
  (unless (and (dict? versions) (andmap dict? (map cdr versions)))
    (raise-argument-error 'VersionedStruct "dict of dicts" versions))
  (inherit-field fields key-index)
  (field [forced-version #f])
  
  (define/public-final (force-version! version)
    (set! forced-version version))
  
  (define/override (decode stream [parent #f] [length 0])
    (define res (send this _setup stream parent length))
    (define version (cond
                      [forced-version] ; for testing purposes: pass an explicit version
                      [(integer? version-resolver) version-resolver] 
                      [(procedure? version-resolver) (version-resolver parent)]
                      [(RestructureBase? version-resolver) (send version-resolver decode stream)]
                      [else (raise-argument-error 'VersionedStruct:decode "way of finding version" version-resolver)]))
    (hash-set! res 'version version)
    (define assocs (dict-ref versions version (λ () (raise-argument-error 'VersionedStruct:decode "valid version key" version))))
    (send this update-fields! assocs)
    (cond
      [(VersionedStruct? fields) (send fields decode stream parent)]
      [else
       (send this _parseFields stream res fields)
       (send this process res stream)
       res])))

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
   (define es (+EncodeStream))
   (send vs encode es (send vs decode bs))
   (check-equal? (send es dump) bs)))
