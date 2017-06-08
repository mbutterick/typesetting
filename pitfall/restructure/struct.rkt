#lang restructure/racket
(require racket/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#

(define-subclass RBase (RStruct assocs)
  (field [key-index (map car assocs)]
         [fields (mhash)])
  (for ([(k v) (in-dict assocs)])
    (hash-set! fields k v))

  (define/override (decode stream [parent #f] [length 0])
    (define res (_setup stream parent length))
    (_parseFields stream res fields)
    #;(hash-set! (hash-ref res '_props) '_currentOffset (· stream pos))
    res)

  (define/override (encode stream val [parent #f])
    (for ([key (in-list key-index)])
      (send (hash-ref fields key) encode stream (hash-ref val key))))

  (define/private (_setup stream parent length)
    (define res (mhasheq))

    ;; define hidden properties
    #;(hash-set! res '_props
               (mhasheq 'parent (mhasheq 'value parent)
                        '_startOffset (mhasheq 'value (· stream pos))
                        '_currentOffset (mhasheq 'value 0 'writable #t)
                        '_length (mhasheq 'value length)))
    res)

  (define/private (_parseFields stream res field)
    (for ([key (in-list key-index)])
      (define hashvalue (hash-ref fields key))
      (define val
        (if (procedure? hashvalue)
            (hashvalue res)
            (send hashvalue decode stream res)))
      (hash-set! res key val)))

  )
