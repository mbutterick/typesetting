#lang debug racket/base
(require racket/private/generic-methods
         racket/dict
         racket/port
         racket/class
         "generic.rkt")
(provide (all-defined-out))

(define (dict-ref* d . keys)
  (for/fold ([d d])
            ([k (in-list keys)])
    (dict-ref d k)))

(define (pos p [new-pos #f])
  (when new-pos
    (file-position p new-pos))
  (file-position p))

(define xenomorphic<%>
  (interface* ()
              ([(generic-property gen:xenomorphic)
                (generic-method-table
                 gen:xenomorphic                                      
                 (define (decode xo [port-arg (current-input-port)] #:parent [parent #f])
                   (define port
                     (cond
                       [(input-port? port-arg) port-arg]
                       [(bytes? port-arg) (open-input-bytes port-arg)]
                       [else (raise-argument-error 'decode "byte string or input port" port-arg)]))
                   (send xo decode port parent))
                                      
                 (define (encode xo val [port-arg (current-output-port)]
                                 #:parent [parent #f])
                   (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
                   (send xo xxencode val port parent)
                   (unless port-arg (get-output-bytes port)))
                                      
                 (define (size xo [val #f] #:parent [parent #f])
                   (send xo xxsize val parent)))])))

(define (xenomorphic-type? x) (is-a? x xenobase%))

(define-syntax-rule (generate-subclass CLASS PRE-ENCODE-PROC POST-DECODE-PROC)
  (cond
    [(and PRE-ENCODE-PROC POST-DECODE-PROC)
     (class CLASS
       (super-new)
       (define/override (pre-encode x) (super pre-encode (PRE-ENCODE-PROC x)))
       (define/override (post-decode x) (POST-DECODE-PROC (super post-decode x))))]
    [PRE-ENCODE-PROC
     (class CLASS
       (super-new)
       (define/override (pre-encode x) (super pre-encode (PRE-ENCODE-PROC x))))]
    [POST-DECODE-PROC
     (class CLASS
       (super-new)
       (define/override (post-decode x) (POST-DECODE-PROC (super post-decode x))))]
    [else CLASS]))

(define xenobase%
  (class* object% (xenomorphic<%>)
    (super-new)
    
    (define/pubment (xxdecode input-port [parent #f])
      (post-decode (inner (error 'xxdecode-not-augmented) xxdecode input-port parent)))

    (define/public (decode input-port [parent #f])
      (xxdecode input-port parent))
    
    (define/pubment (xxencode val output-port [parent #f])
      (define encode-result (inner (error 'xxencode-not-augmented) xxencode (pre-encode val) output-port parent))
      (when (bytes? encode-result) (write-bytes encode-result output-port)))
    
    (define/pubment (xxsize [val #f] [parent #f] . args)
      (define size (inner 0 xxsize val parent . args))
      (unless (and (integer? size) (not (negative? size)))
        (raise-argument-error 'size "nonnegative integer" size))
      size)
    
    (define/public (post-decode val) val)
    (define/public (pre-encode val) val)))