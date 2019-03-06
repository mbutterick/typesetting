#lang racket/base
(require racket/class racket/dict racket/match)
(provide (all-defined-out))

(struct x:ptr (type val parent) #:transparent)

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

(define (dict->mutable-hash x)
  (define h (make-hasheq))
  (for ([(k v) (in-dict x)]
        #:unless (memq k private-keys))
    (hash-set! h k v))
  h)

(define (hash-ref* d . keys)
  (for/fold ([d d])
            ([k (in-list keys)])
    (hash-ref d k)))

(define (pos p [new-pos #f])
  (when new-pos
    (file-position p new-pos))
  (file-position p))

(define (decode xo [port-arg (current-input-port)] #:parent [parent #f] . args)
  (define port
    (cond
      [(input-port? port-arg) port-arg]
      [(bytes? port-arg) (open-input-bytes port-arg)]
      [else (raise-argument-error 'decode "byte string or input port" port-arg)]))
  (send xo decode port parent . args))
                                      
(define (encode xo val [port-arg (current-output-port)]
                #:parent [parent #f]
                . args)
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (send xo encode val port parent . args)
  (unless port-arg (get-output-bytes port)))
                                      
(define (size xo [val #f] #:parent [parent #f] . args)
  (send xo size val parent . args))

(define (xenomorphic-type? x) (is-a? x x:base%))
(define xenomorphic? xenomorphic-type?)

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

(define x:base%
  (class object%
    (super-new)
    
    (define/pubment (decode input-port [parent #f] . args)
      (post-decode (inner (error 'xenomorph (format "decode not augmented in ~a" this)) decode input-port parent . args)))
    
    (define/pubment (encode val output-port [parent #f] . args)
      (match (inner (error 'xenomorph (format "encode not augmented in ~a" this)) encode (pre-encode val) output-port parent . args)
        [(? bytes? encode-result) (write-bytes encode-result output-port)]
        [_ (void)]))
    
    (define/pubment (size [val #f] [parent #f] . args)
      (match (inner 0 size val parent . args)
        [(? exact-nonnegative-integer? size) size]
        [other (raise-argument-error 'size "nonnegative integer" other)]))
    
    (define/public (post-decode val) val)
    (define/public (pre-encode val) val)))