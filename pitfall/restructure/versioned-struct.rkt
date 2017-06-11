#lang restructure/racket
(require racket/dict "struct.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/VersionedStruct.coffee
|#

(define-subclass RStruct (RVersionedStruct type [versions (dictify)])
  (define/override (decode stream [parent #f] [length 0] #:version [maybe-version #f])
    (define res (send this _setup stream parent length))
    (define version (cond
                      [maybe-version] ; for testing purposes: pass an explicit version
                      [(procedure? type) (type parent)]
                      [(is-a? type RBase) (send type decode stream)]
                      [else (raise-argument-error 'decode "way of finding version" type)]))
    (hash-set! res 'version version)
    (set-field! fields this (dict-ref versions version (λ () (raise-argument-error 'RVersionedStruct:decode "valid version key" version))))
    (send this make-key-index! (· this fields))
    (cond
      [(is-a? (· this fields) RVersionedStruct) (send (· this fields) decode stream parent)]
      [else
       (send this _parseFields stream res (· this fields))
       (send this process res stream)
       res]))
  )