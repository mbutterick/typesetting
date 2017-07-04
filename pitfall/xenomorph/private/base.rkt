#lang racket/base
(require racket/class sugar/class racket/generic racket/private/generic-methods)
(provide (all-defined-out))

(define-generics codable
  (decode codable #:parent [parent] [stream])
  (encode codable [val] [stream] #:parent [parent]))


(define codable<%>
  (interface* ()
              ([(generic-property gen:codable)
                (generic-method-table gen:codable
                                      (define (decode o [stream (current-input-port)] #:parent [parent #f]) (send o decode stream parent))
                                      (define (encode o  [val #f] [stream (current-output-port)] #:parent [parent #f]) (send o encode stream val parent)))])))


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
  (class* object% (codable<%> sizable<%> dumpable<%>)
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