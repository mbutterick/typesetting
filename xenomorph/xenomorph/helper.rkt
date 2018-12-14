#lang racket/base
(require racket/generic
         racket/dict
         racket/port)
(provide (all-defined-out))

(define (->input-port arg)
  (cond
    [(bytes? arg) (open-input-bytes arg)]
    [(input-port? arg) arg]
    [else (raise-argument-error '->input-port "byte string or input port" arg)]))

(define private-keys '(parent _startOffset _currentOffset _length))
(define (dump-mutable x)
  (define h (make-hasheq))
  (for ([(k v) (in-dict (dump x))])
    (hash-set! h k v))
  h)

(define (dump x)
  (cond
    [(input-port? x) (port->bytes x)]
    [(output-port? x) (get-output-bytes x)]
    [(dict? x) (for/hasheq ([(k v) (in-dict x)]
                            #:unless (memq k private-keys))
                 (values k v))]
    [(list? x) (map dump x)]
    [else x]))

(define (pos p [new-pos #f])
  (when new-pos
    (file-position p new-pos))
  (file-position p))

(struct xbase ([pre-encode #:auto] [post-decode #:auto]) #:transparent #:mutable
  #:auto-value values)

(define (pre-encode xb val)
  ((xbase-pre-encode xb) val))

(define (set-pre-encode! xb func)
  (set-xbase-pre-encode! xb func))

(define (post-decode xb val)
  ((xbase-post-decode xb) val))

(define (set-post-decode! xb func)
  (set-xbase-post-decode! xb func))

(define-syntax-rule (define/post-decode (ID X VAL . ARGS) . BODY)
  (define (ID X VAL . ARGS) (post-decode X (let () . BODY))))

(define-syntax-rule (define/pre-encode (ID X VAL . ARGS) . BODY)
  (define (ID X val-in . ARGS) (let ([VAL (pre-encode X val-in)]) . BODY)))

(define-syntax-rule (define/finalize-size ID+ARGS . BODY) (define ID+ARGS (finalize-size (let () . BODY)))) 

(define-generics xenomorphic
  (encode xenomorphic val [port] #:parent [parent])
  (decode xenomorphic [port] #:parent [parent])
  (size xenomorphic [item] #:parent [parent]))

(define (finalize-size size)
  (cond
    [(void? size) 0]
    [(and (integer? size) (not (negative? size))) size]
    [else (raise-argument-error 'size "nonnegative integer" size)]))