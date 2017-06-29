#lang restructure/racket
(require racket/dict "struct.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/VersionedStruct.coffee
|#

(define-subclass Struct (VersionedStruct type [versions (dictify)])
  
  (unless ((disjoin integer? procedure? RestructureBase? symbol?) type)
    (raise-argument-error 'VersionedStruct "integer, function, symbol, or Restructure object" type))
  (unless (and (dict? versions) (andmap (λ (val) (or (dict? val) (Struct? val))) (map cdr versions)))
    (raise-argument-error 'VersionedStruct "dict of dicts or Structs" versions))

  (inherit _setup  _parseFields process)
  (inherit-field fields)
  (field [forced-version #f]
         [versionGetter void]
         [versionSetter void])

  (when (symbol? type) ; instead of string
    (set-field! versionGetter this (λ (parent) (ref parent type)))
    (set-field! versionSetter this (λ (parent version) (ref-set! parent type version))))

  (define/override (decode stream [parent #f] [length 0])
    (define res (_setup stream parent length))
    
    (ref-set! res 'version
              (cond
                [forced-version] ; for testing purposes: pass an explicit version
                [(symbol? type) (unless parent
                                  (raise-argument-error 'VersionedStruct:decode "valid parent" parent))
                                (versionGetter parent)]
                [else (send type decode stream)]))

    (when (dict-ref versions 'header #f)
      (_parseFields stream res (ref versions 'header)))
    
    (define fields (dict-ref versions (ref res 'version) (λ () (raise-argument-error 'VersionedStruct:decode "valid version key" (cons version (· this versions))))))

    (cond
      [(VersionedStruct? fields) (send fields decode stream parent)]
      [else
       (_parseFields stream res fields)
       (process res stream)
       res]))
  
  (define/public-final (force-version! version)
    (set! forced-version version))  

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

  
  (define/override (size [val (mhash)] [parent #f] [includePointers #t])
    (unless (or val forced-version)
      (error 'VersionedStruct-cannot-compute-size))

    (define ctx (mhash 'parent parent
                       'val val
                       'pointerSize 0))

    (define size 0)
    (when (not (string? type))
      (increment! size (send type size (ref val 'version) ctx)))

    (when (ref versions 'header)
      (increment! size
                  (for/sum ([(key type) (in-dict (ref versions 'header))])
                    (send type size (ref val key) ctx))))
    
    (define fields (dict-ref versions (ref val 'version) (λ () (raise-argument-error 'VersionedStruct:encode "valid version key" version))))

    (increment! size
                (for/sum ([(key type) (in-dict (ref versions 'header))])
                  (send type size (ref val key) ctx)))

    (when includePointers
      (increment! size (ref ctx 'pointerSize)))

    size))

#;(test-module
   (require "number.rkt")
   (define (random-pick xs) (list-ref xs (random (length xs))))
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


