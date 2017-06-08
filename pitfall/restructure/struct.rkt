#lang restructure/racket
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Struct.coffee
|#

(define-subclass RBase (RStruct [fields (mhash)])

  (define/override (decode stream [parent #f] [length 0])
    (define res (_setup stream parent length))
    (_parseFields stream res fields)
    (hash-set! (hash-ref res '_props) '_currentOffset (port-position stream))
    res)

  (define/override (encode stream val [parent #f])
    (for ([(key type) (in-hash fields)])
      (send type encode stream (hash-ref val key))))

  (define/private (_setup stream parent length)
    (define res (mhasheq))

    ;; define hidden properties
    (hash-set! res '_props
               (mhasheq 'parent (mhasheq 'value parent)
                      '_startOffset (mhasheq 'value (port-position stream))
                      '_currentOffset (mhasheq 'value 0 'writable #t)
                      '_length (mhasheq 'value length)))
    res)

  (define/private (_parseFields stream res field)
    (for ([(key hashvalue) (in-hash fields)])
      (define val
        (if (procedure? hashvalue)
            (hashvalue res)
            (send hashvalue decode stream res)))
      (hash-set! res key val)))

  )
