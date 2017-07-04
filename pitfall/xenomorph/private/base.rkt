#lang racket/base
(require racket/class sugar/class racket/generic racket/private/generic-methods "generic.rkt")
(require sugar/debug)
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


(define xenomorph-base%
  (class* object% (codable<%> sizable<%> dumpable<%>)
    (super-new)
    (field [_hash (make-hash)]
           [_list null])

    (define/pubment (decode port [parent #f])
      (when parent (unless (indexable? parent)
                     (raise-argument-error 'Xenomorph "indexable" parent)))
      (define ip (cond
                   [(bytes? port) (open-input-bytes port)]
                   [(input-port? port) port]
                   [else (raise-argument-error 'Xenomorph "bytes or input port" port)]))
      (post-decode (inner (void) decode ip parent)))
    
    (define/pubment (encode port val-in [parent #f])
      #;(report* port val-in parent)
      (define val (pre-encode val-in))
      (when parent (unless (indexable? parent)
                     (raise-argument-error 'Xenomorph "indexable" parent)))
      (define op (cond
                   [(output-port? port) port]
                   [(not port) (open-output-bytes)]
                   [else (raise-argument-error 'Xenomorph "output port or #f" port)]))
      (define encode-result (inner (void) encode port val parent))
      (when (bytes? encode-result)
        (write-bytes encode-result op))
      (when (not port) (get-output-bytes op)))
    
    (define/pubment (size [val #f] [parent #f])
      (when parent (unless (indexable? parent)
                     (raise-argument-error 'Xenomorph "indexable" parent)))
      (define result (inner (void) size val parent))
      (when result (unless (and (integer? result) (not (negative? result)))
                     (raise-argument-error 'Xenomorph "integer" result)))
      result)
    
    (define/public (post-decode val) val)
    (define/public (pre-encode val) val)
    (define/public (dump) (void))))

(define-class-predicates xenomorph-base%)

(define-subclass xenomorph-base% (RestructureBase))
(define-subclass RestructureBase (Streamcoder))

