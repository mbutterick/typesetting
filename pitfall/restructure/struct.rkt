#lang restructure/racket
(require racket/dict "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#

(define-subclass Streamcoder (Struct [fields (dictify)])
  (field [res #f])
  
  (unless ((disjoin assocs? VersionedStruct?) fields)
    (raise-argument-error 'Struct "assocs or Versioned Struct" fields))

  (define/augride (decode stream [parent #f] [length 0])
    (set! res (_setup stream parent length))
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
      (report key)
      (define val
        (if (procedure? type)
            (type res)
            (send type decode stream this)))
      (hash-set! res key val)))

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
                   
 


#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/VersionedStruct.coffee
|#

(define-subclass Struct (VersionedStruct type [versions (dictify)])
  (inherit-field res)
  (unless ((disjoin integer? procedure? RestructureBase? symbol?) type)
    (raise-argument-error 'VersionedStruct "integer, function, symbol, or Restructure object" type))
  (unless (and (dict? versions) (andmap (λ (val) (or (dict? val) (Struct? val))) (map cdr versions)))
    (raise-argument-error 'VersionedStruct "dict of dicts or Structs" versions))
  (inherit-field fields)
  (field [forced-version #f])
  
  (define/public-final (force-version! version)
    (set! forced-version version))

  (define/public (resolve-version [stream #f] [parent #f])
    (cond
      [forced-version] ; for testing purposes: pass an explicit version
      [(integer? type) type]
      [(symbol? type)
       ;; find the first Struct in the chain of ancestors
       ;; with the target key
       (let loop ([x parent])
         (cond
           [(and x (Struct? x) (dict-ref (· x res) type #f))]
           [(· x parent) => loop]
           [else #f]))]
      [(and (procedure? type) (positive? (procedure-arity type))) (type parent)]
      [(RestructureBase? type) (send type decode stream)]
      [else (raise-argument-error 'VersionedStruct:resolve-version "way of finding version" type)]))
  
  (define/override (decode stream [parent #f] [length 0])
    (set! res (send this _setup stream parent length))
    (report res 'versioned-struct-res)
    (define version (resolve-version stream parent))
    (hash-set! res 'version version)
    (define fields (dict-ref versions version (λ () (raise-argument-error 'VersionedStruct:decode "valid version key" (cons version (· this versions))))))
    (cond
      [(VersionedStruct? fields) (send fields decode stream parent)]
      [else
       (report res 'whatigot)
       (send this _parseFields stream res fields)
       (send this process res stream)
       res]))

  (define/override (encode stream input-hash [parent #f])
    (unless (hash? input-hash)
      (raise-argument-error 'Struct:encode "hash" input-hash))

    (send this preEncode input-hash stream) ; preEncode goes first, because it might bring input hash into compliance

    (define fields (dict-ref versions (· input-hash version) (λ () (raise-argument-error 'VersionedStruct:encode "valid version key" version))))

    (unless (andmap (λ (key) (member key (hash-keys input-hash))) (dict-keys fields))
      (raise-argument-error 'Struct:encode (format "hash that contains superset of Struct keys: ~a" (dict-keys fields)) (hash-keys input-hash)))

    (cond
      [(dict? fields)
       (for* ([(key type) (in-dict fields)])
         (send type encode stream (hash-ref input-hash key)))]
      [else (send fields encode stream input-hash parent)]))

  
  (define/override (size [input-hash (mhash)] [parent #f] [includePointers #t])
    (when (and (not input-hash) (not forced-version))
      (error 'VersionedStruct-cannot-compute-size))
    (define version (resolve-version #f parent))
    (define fields (dict-ref versions version (λ () (raise-argument-error 'VersionedStruct:size "valid version key" version))))
    (cond
      [(dict? fields)
       (for/sum ([(key type) (in-dict fields)])
         (define val (hash-ref input-hash key #f))
         (define args (if val (list val) empty))
         (send type size . args))]
      [else (send fields size input-hash parent includePointers)])))

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
 (check-equal? (send vs size) 6)
 (define s2 (+Struct (dictify 'a vs)))
 (check-equal? (send s2 size) 6)
 (define vs2 (+VersionedStruct (λ (p) 2) (dictify 1 vs 2 vs)))
 (check-equal? (send vs2 size) 6)

 )


