#lang racket/base
(require racket/class sugar/class racket/generic racket/private/generic-methods)
(provide (all-defined-out))

(define-generics encodable
  (decode encodable stream [parent])
  (encode encodable stream [val] [parent]))


(define encodable<%>
  (interface* ()
              ([(generic-property gen:encodable)
                (generic-method-table gen:encodable
                                      (define (decode o stream [parent #f]) (send o decode stream parent))
                                      (define (encode o stream [val #f] [parent #f]) (send o encode stream val parent)))])))


(define-generics sizable
  (size sizable [val] [parent]))

(define sizable<%>
  (interface* ()
              ([(generic-property gen:sizable)
                (generic-method-table gen:sizable
                                      (define (size o [val #f] [parent #f]) (send o size val parent)))])))


(define-generics dumpable
  (dump dumpable))

(define dumpable<%>
  (interface* ()
              ([(generic-property gen:dumpable)
                (generic-method-table gen:dumpable
                                      (define (dump o) (send o dump)))])))


(define RestructureBase
  (class* object% (encodable<%> sizable<%> dumpable<%>)
    (super-new)
    (field [_hash (make-hash)]
           [_list null])
    (define/public (decode stream . args) (void))
    (define/public (encode . xs) (void))
    (define/public (size . xs) (void))
    (define/public (process . args) (void))
    (define/public (preEncode . args) (void))
    (define/public (dump) (void))))

(define-class-predicates RestructureBase)