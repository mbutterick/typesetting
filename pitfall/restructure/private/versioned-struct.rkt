#lang reader (submod "racket.rkt" reader)
(require racket/dict "struct.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbuttrackerick/restructure/blob/master/src/VersionedStruct.coffee
|#


(define-subclass Struct (VersionedStruct type [versions (dictify)])
  
  (unless ((disjoin integer? procedure? RestructureBase? symbol?) type)
    (raise-argument-error 'VersionedStruct "integer, function, symbol, or Restructure object" type))
  (unless (and (dict? versions) (andmap (λ (val) (or (dict? val) (Struct? val))) (map cdr versions)))
    (raise-argument-error 'VersionedStruct "dict of dicts or Structs" versions))

  (inherit _setup  _parse-fields process)
  (inherit-field fields)
  (field [forced-version #f]
         [versionGetter void]
         [versionSetter void])

  (when (or (key? type) (procedure? type))
    (set-field! versionGetter this (if (procedure? type)
                                       type
                                       (λ (parent) (ref parent type))))
    (set-field! versionSetter this (if (procedure? type)
                                       type
                                       (λ (parent version) (ref-set! parent type version)))))

  (define/override (decode stream [parent #f] [length 0])
    (define res (_setup stream parent length))

    (ref-set! res 'version
              (cond
                [forced-version] ; for testing purposes: pass an explicit version
                [(or (key? type) (procedure? type))
                 (unless parent
                                  (raise-argument-error 'VersionedStruct:decode "valid parent" parent))
                                (versionGetter parent)]
                [else (send type decode stream)]))

    (when (ref versions 'header)
      (_parse-fields stream res (ref versions 'header)))
    
    (define fields (or (ref versions (ref res 'version)) (raise-argument-error 'VersionedStruct:decode "valid version key" (cons version (· this versions)))))

    
    (cond
      [(VersionedStruct? fields) (send fields decode stream parent)]
      [else
       (_parse-fields stream res fields)
       (process res stream)
       res]))
  
  (define/public-final (force-version! version)
    (set! forced-version version))  

  (define/override (encode stream val [parent #f])
    (unless (hash? val)
      (raise-argument-error 'Struct:encode "hash" val))

    (send this preEncode val stream) ; preEncode goes first, because it might bring input hash into compliance

    (define ctx (mhash 'pointers empty
                       'startOffset (· stream pos)
                       'parent parent
                       'val val
                       'pointerSize 0))

    (ref-set! ctx 'pointerOffset (+ (· stream pos) (size val ctx #f)))

    (when (not (or (key? type) (procedure? type)))
      (send type encode stream (or forced-version (· val version))))

    (when (ref versions 'header)
      (for ([(key type) (in-dict (ref versions 'header))])
           (send type encode stream (ref val key) ctx)))

    (define fields (or (ref versions (or forced-version (· val version))) (raise-argument-error 'VersionedStruct:encode "valid version key" version)))

    (unless (andmap (λ (key) (member key (ref-keys val))) (ref-keys fields))
      (raise-argument-error 'VersionedStruct:encode (format "hash that contains superset of Struct keys: ~a" (dict-keys fields)) (hash-keys val)))

    (for ([(key type) (in-dict fields)])
         (send type encode stream (ref val key) ctx))

    (for ([ptr (in-list (ref ctx 'pointers))])
         (send (ref ptr 'type) encode stream (ref ptr 'val) (ref ptr 'parent))))

  
  (define/override (size [val #f] [parent #f] [includePointers #t])
    (unless (or val forced-version)
      (raise-argument-error 'VersionedStruct:size "value" val))

    (define ctx (mhash 'parent parent
                       'val val
                       'pointerSize 0))

    (define size 0)
    (when (not (or (key? type) (procedure? type)))
      (increment! size (send type size (or forced-version (ref val 'version)) ctx)))
 
    (when (ref versions 'header)
      (increment! size
                  (for/sum ([(key type) (in-dict (ref versions 'header))])
                           (send type size (and val (ref val key)) ctx))))
    
    (define fields (or (ref versions (or forced-version (ref val 'version))) (raise-argument-error 'VersionedStruct:encode "valid version key" version)))

    (increment! size
                (for/sum ([(key type) (in-dict fields)])
                         (send type size (ref val key) ctx)))

    (when includePointers
      (increment! size (ref ctx 'pointerSize)))

    size))

#;(test-module
   (require "number.rkt")
   (define (random-pick xs) (list-ref xs (random (length xs))))
   (check-exn exn:fail:contract? (λ () (+VersionedStruct 42 42)))

   ;; make random versioned structs and make sure we can round trip
   #;(for ([i (in-range 1)])
          (define field-types (for/list ([i (in-range 1)])
                                        (random-pick (list uint8 uint16be uint16le uint32be uint32le double))))
          (define num-versions 20)
          (define which-struct (random num-versions))
          (define struct-versions (for/list ([v (in-range num-versions)])
                                            (cons v (for/list ([num-type (in-list field-types)])
                                                              (cons (gensym) num-type)))))
          (define vs (+VersionedStruct which-struct struct-versions))
          (define struct-size (for/sum ([num-type (in-list (map cdr (ref struct-versions which-struct)))])
                                       (send num-type size)))
          (define bs (apply bytes (for/list ([i (in-range struct-size)])
                                            (random 256))))
          (check-equal? (send vs encode #f (send vs decode bs)) bs))

   (define s (+Struct (dictify 'a uint8 'b uint8 'c uint8)))
   (check-equal? (send s size) 3)
   (define vs (+VersionedStruct uint8 (dictify 1 (dictify 'd s) 2 (dictify 'e s 'f s))))
   (send vs force-version! 1)
   (check-equal? (send vs size) 6)
   #|
   (define s2 (+Struct (dictify 'a vs)))
   (check-equal? (send s2 size) 6)
   (define vs2 (+VersionedStruct (λ (p) 2) (dictify 1 vs 2 vs)))
   (check-equal? (send vs2 size) 6)
|#
   )


