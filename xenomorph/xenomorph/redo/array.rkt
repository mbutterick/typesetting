#lang debug racket/base
(require racket/dict racket/sequence "base.rkt" "number.rkt" "util.rkt" sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Array.coffee
|#

(define (xarray-decode xa [port-arg (current-input-port)] #:parent [parent #f])
  (define port (->input-port port-arg))
  (define ctx (if (xint? (xarray-len xa))
                  (mhasheq 'parent parent
                           '_startOffset (pos port)
                           '_currentOffset 0
                           '_length (xarray-len xa))
                  parent))
  (define decoded-len (resolve-length (xarray-len xa) port parent))
  (cond
    [(or (not decoded-len) (eq? (xarray-length-type xa) 'bytes))
     (define end-pos (cond
                       ;; decoded-len is byte length
                       [decoded-len (+ (pos port) decoded-len)]
                       ;; no decoded-len, but parent has length
                       [(and parent (not (zero? (dict-ref parent '_length)))) (+ (dict-ref parent '_startOffset) (dict-ref parent '_length))]
                       ;; no decoded-len or parent, so consume whole stream
                       [else +inf.0]))
     (for/list ([i (in-naturals)]
                #:break (or (eof-object? (peek-byte port)) (= (pos port) end-pos)))
       (decode (xarray-type xa) port #:parent ctx))]
    ;; we have decoded-len, which is treated as count of items
    [else (for/list ([i (in-range decoded-len)])
            (decode (xarray-type xa) port #:parent ctx))]))

(define (xarray-encode xa array [port-arg (current-output-port)] #:parent [parent #f])
  (unless (sequence? array)
    (raise-argument-error 'xarray-encode "sequence" array))
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (define (encode-items ctx)
    ;; todo: should array with fixed length stop encoding after it reaches max?
    ;; cf. xstring, which rejects input that is too big for fixed length.
    (let* (#;[items (sequence->list array)]
           #;[item-count (length items)]
           #;[max-items (if (number? (xarray-len xa)) (xarray-len xa) item-count)])
      (for ([item array])
        (encode (xarray-type xa) item port #:parent ctx))))

  (cond
    [(xint? (xarray-len xa))
     (define ctx (mhash 'pointers null
                        'startOffset (pos port)
                        'parent parent))
     (dict-set! ctx 'pointerOffset (+ (pos port) (size xa array ctx)))
     (encode (xarray-len xa) (length array) port) ; encode length at front
     (encode-items ctx)
     (for ([ptr (in-list (dict-ref ctx 'pointers))]) ; encode pointer data at end
       (encode (dict-ref ptr 'type) (dict-ref ptr 'val) port))]
    [else (encode-items parent)])
  (unless port-arg (get-output-bytes port)))

(define (xarray-size xa [val #f] [ctx #f])
  (when val (unless (sequence? val)
              (raise-argument-error 'xarray-size "sequence" val)))
  (cond
    [val (let-values ([(ctx len-size) (if (xint? (xarray-len xa))
                                          (values (mhasheq 'parent ctx) (size (xarray-len xa)))
                                          (values ctx 0))])
           (+ len-size (for/sum ([item val])
                         (size (xarray-type xa) item ctx))))]
    [else (let ([item-count (resolve-length (xarray-len xa) #f ctx)]
                [item-size (size (xarray-type xa) #f ctx)])
            (* item-size item-count))]))

(struct xarray (type len length-type) #:transparent
  #:methods gen:xenomorphic
  [(define decode xarray-decode)
   (define encode xarray-encode)
   (define size xarray-size)])

(define (+xarray type [len #f] [length-type 'count])
  (unless (xenomorphic? type)
    (raise-argument-error '+xarray "xenomorphic type" type))
  (unless (length-resolvable? len)
    (raise-argument-error '+xarray "length-resolvable?" len))
  (unless (memq length-type '(bytes count))
    (raise-argument-error '+xarray "'bytes or 'count" length-type))
  (xarray type len length-type))
  

(module+ test
  (require rackunit)
  (check-equal? (decode (+xarray uint16be 3) #"ABCDEF") '(16706 17220 17734))
  (check-equal? (encode (+xarray uint16be 3) '(16706 17220 17734) #f) #"ABCDEF")
  (check-equal? (size (+xarray uint16be) '(1 2 3)) 6)
  (check-equal? (size (+xarray doublebe) '(1 2 3 4 5)) 40))
