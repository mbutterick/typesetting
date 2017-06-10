#lang restructure/racket
(require racket/dict "struct.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/VersionedStruct.coffee
|#

(define-subclass RStruct (RVersionedStruct type [versions (dictify)])
  
  (define/override (decode stream [parent #f] [length 0])
    (define res (send this _setup stream parent length))
    (define version (cond
                      [(procedure? type) (type parent)]
                      [(is-a? type RBase) (send type decode stream)]
                      [else (raise-argument-error 'decode "way of finding version" type)]))
    (report version 'yay)
    #;(_parseFields stream res fields)
    #;(send this process res stream)
    res))