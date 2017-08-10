#lang racket/base
(require racket/class sugar/class racket/generic racket/private/generic-methods "generic.rkt" racket/port racket/dict racket/function)
(require sugar/debug)
(provide (all-defined-out))
(define-generics posable
  (pos posable [new-pos])
  #:defaults
  ([port? (define (pos p [new-pos #f]) (when new-pos
                                         (file-position p new-pos))
            (file-position p))]))

(define posable<%>
  (interface* ()
              ([(generic-property gen:posable)
                (generic-method-table gen:posable
                                      (define (pos o [new-pos #f]) (send o pos new-pos)))])))

(define-generics codable
  (decode codable #:parent [parent] [stream])
  (encode codable [val] [stream] #:parent [parent]))


(define codable<%>
  (interface* ()
              ([(generic-property gen:codable)
                (generic-method-table gen:codable
                                      (define (decode o [port (current-input-port)] #:parent [parent #f])
                                        (send o decode port parent))
                                      (define (encode o  [val #f] [port (current-output-port)] #:parent [parent #f])
                                        (when (port? val)
                                          (raise-argument-error 'encode "encodable value" val))
                                        (send o encode port val parent)))])))


(define-generics sizable
  (size sizable [val] [parent]))

(define sizable<%>
  (interface* ()
              ([(generic-property gen:sizable)
                (generic-method-table gen:sizable
                                      (define (size o [val #f] [parent #f]) (send o size val parent)))])))

(define (dump x)
  (define (dump-dict x)
    (for/list ([(k v) (in-dict x)])
              (cons (dump k) (dump v))))
  (let loop ([x x])
    (cond
      [(input-port? x) (port->bytes x)]
      [(output-port? x) (get-output-bytes x)]
      [(and (object? x)
            (memq 'dump (interface->method-names (object-interface x)))) (send x dump)]
      [(dict? x) (dump-dict x)]
      [(list? x) (map loop x)]
      [else x])))

#;(define dumpable<%>
    (interface* ()
                ([(generic-property gen:dumpable)
                  (generic-method-table gen:dumpable
                                        (define (dump o) (send o dump)))])))

(define (symbol-append . syms)
  (string->symbol (apply string-append (map symbol->string syms))))

(define xenomorph-base%
  (class* object% (codable<%> sizable<%>)
    (super-new)
    (field [_hash (make-hash)]
           [_list null])

    (define/pubment (decode port [parent #f] . args)
      (when parent (unless (indexable? parent)
                     (raise-argument-error (symbol-append (get-class-name) ':decode) "indexable" parent)))
      (define ip (cond
                   [(bytes? port) (open-input-bytes port)]
                   [(input-port? port) port]
                   [else (raise-argument-error (symbol-append (get-class-name) ':decode) "bytes or input port" port)]))
      (post-decode (inner (void) decode ip parent) port parent . args))
    
    (define/pubment (encode port val-in [parent #f] . args)
      #;(report* port val-in parent)
      (define val (pre-encode val-in port))
      (when parent (unless (indexable? parent)
                     (raise-argument-error (symbol-append (get-class-name) ':encode) "indexable" parent)))
      (define op (cond
                   [(output-port? port) port]
                   [(not port) (open-output-bytes)]
                   [else (raise-argument-error 'Xenomorph "output port or #f" port)]))
      (define encode-result (inner (void) encode op val parent . args))
      (when (bytes? encode-result)
        (write-bytes encode-result op))
      (when (not port) (get-output-bytes op)))
    
    (define/pubment (size [val #f] [parent #f] . args)
      (when parent (unless (indexable? parent)
                     (raise-argument-error (symbol-append (get-class-name) ':size) "indexable" parent)))
      (define result (inner (void) size val parent . args))
      (cond
        [(void? result) 0]
        [(and (integer? result) (not (negative? result))) result]
        [else (raise-argument-error (symbol-append (get-class-name) ':size) "nonnegative integer" result)]))

    (define/public (get-class-name) (define-values (name _) (object-info this))
      (or name 'Xenomorph))
    
    (define/public (post-decode val . _) val)
    (define/public (pre-encode val . _) val)
    (define/public (dump) (void))))

(define-class-predicates xenomorph-base%)

(define-subclass xenomorph-base% (RestructureBase))
(define-subclass RestructureBase (Streamcoder))

