#lang racket/base
(require racket/private/generic-methods
         racket/class
         "generic.rkt")
(provide (all-defined-out))

(struct x:ptr (type val parent) #:transparent #:mutable)

(define x:version-key 'x:version)
(define x:start-offset-key 'x:start-offset)
(define x:current-offset-key 'x:current-offset)
(define x:length-key 'x:length)
(define x:parent-key 'x:parent)
(define x:pointer-size-key 'x:ptr-size)
(define x:pointers-key 'x:pointers)
(define x:pointer-offset-key 'x:ptr-offset)
(define x:pointer-type-key 'x:ptr-type)
(define x:val-key 'x:val)

(define private-keys (list x:parent-key x:start-offset-key x:current-offset-key x:length-key x:pointer-size-key
                           x:pointers-key x:pointer-offset-key x:pointer-type-key x:val-key))

(define (hash-ref* d . keys)
  (for/fold ([d d])
            ([k (in-list keys)])
    (hash-ref d k)))

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
                   (send xo x:encode val port parent)
                   (unless port-arg (get-output-bytes port)))
                                      
                 (define (size xo [val #f] #:parent [parent #f])
                   (send xo x:size val parent)))])))

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
    
    (define/pubment (x:decode input-port [parent #f])
      (post-decode (inner (error 'x:decode-not-augmented) x:decode input-port parent)))

    (define/public (decode input-port [parent #f])
      (x:decode input-port parent))
    
    (define/pubment (x:encode val output-port [parent #f])
      (define encode-result (inner (error 'x:encode-not-augmented) x:encode (pre-encode val) output-port parent))
      (when (bytes? encode-result) (write-bytes encode-result output-port)))
    
    (define/pubment (x:size [val #f] [parent #f] . args)
      (define size (inner 0 x:size val parent . args))
      (unless (and (integer? size) (not (negative? size)))
        (raise-argument-error 'size "nonnegative integer" size))
      size)
    
    (define/public (post-decode val) val)
    (define/public (pre-encode val) val)))