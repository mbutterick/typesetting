#lang restructure/racket
(require racket/dict "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#

(define-subclass Streamcoder (Struct [assocs (dictify)])
  (field [key-index (map car assocs)] ; store the original key order
         [struct-types (mhash)])
  (for ([(k v) (in-dict assocs)])
    (hash-set! struct-types k v))
  
  (define/augride (decode stream [parent #f] [length 0])
    (define res (_setup stream parent length))
    (_parseFields stream res struct-types)
    #;(hash-set! (hash-ref res '_props) '_currentOffset (· stream pos))
    (send this process res stream)
    res)

  (define/augride (encode stream input-hash [parent #f])
    (unless (hash? input-hash)
      (raise-argument-error 'Struct:encode "hash" input-hash))
    (define sorted-input-keys (sort (hash-keys input-hash) #:key symbol->string string<?))
    (define sorted-struct-keys (sort key-index #:key symbol->string string<?))
    (unless (equal? sorted-input-keys sorted-struct-keys)
      (raise-argument-error 'Struct:encode (format "hash with same keys as Struct: ~a" sorted-struct-keys) sorted-input-keys))
    
    (send this preEncode input-hash stream)
    (for* ([key (in-list key-index)] ; iterate over original keys in order
           [struct-type (in-value (hash-ref struct-types key))]
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
    (for/sum ([(key type) (in-hash struct-types)]
              #:when (hash-has-key? val key))
      (send type size (hash-ref val key)))))



#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/VersionedStruct.coffee
|#

(define-subclass Struct (VersionedStruct type [versions (dictify)])
  (define/override (decode stream [parent #f] [length 0] #:version [maybe-version #f])
    (define res (send this _setup stream parent length))
    (define version (cond
                      [maybe-version] ; for testing purposes: pass an explicit version
                      [(procedure? type) (type parent)]
                      [(RestructureBase? type) (send type decode stream)]
                      [else (raise-argument-error 'decode "way of finding version" type)]))
    (hash-set! res 'version version)
    (set-field! fields this (dict-ref versions version (λ () (raise-argument-error 'RVersionedStruct:decode "valid version key" version))))
    (send this make-key-index! (· this fields))
    (cond
      [(VersionedStruct? (· this fields)) (send (· this fields) decode stream parent)]
      [else
       (send this _parseFields stream res (· this fields))
       (send this process res stream)
       res]))
  )
