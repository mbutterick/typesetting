#lang restructure/racket
(require racket/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#

(define-subclass RBase (RStruct [assocs (dictify)])
  (field [key-index #f]
         [fields (mhash)])
  (for ([(k v) (in-dict assocs)])
       (hash-set! fields k v))

  (define/public (make-key-index! [fields assocs])
    (set! key-index (map car fields)))
  (make-key-index!)
  
  (define/override (decode stream [parent #f] [length 0])
    (define res (_setup stream parent length))
    (_parseFields stream res fields)
    #;(hash-set! (hash-ref res '_props) '_currentOffset (· stream pos))
    (send this process res stream)
    res)

  (define/override (encode stream val [parent #f])
    (for ([key (in-list key-index)])
         (send (hash-ref fields key) encode stream (hash-ref val key))))

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

  )
