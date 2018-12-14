#lang racket/base
(require "helper.rkt" "util.rkt" "number.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Buffer.coffee
|#

(define/post-decode (xbuffer-decode xb [port-arg (current-input-port)] #:parent [parent #f])
  (define port (->input-port port-arg))
  (parameterize ([current-input-port port])
    (define decoded-len (resolve-length (xbuffer-len xb) #:parent parent))
    (read-bytes decoded-len)))

(define/pre-encode (xbuffer-encode xb buf [port-arg (current-output-port)] #:parent [parent #f])
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (parameterize ([current-output-port port])
    (unless (bytes? buf)
      (raise-argument-error 'xbuffer-encode "bytes" buf))
    (when (xint? (xbuffer-len xb))
      (encode (xbuffer-len xb) (bytes-length buf)))
    (write-bytes buf)
    (unless port-arg (get-output-bytes port))))

(define/finalize-size (xbuffer-size xb [val #f] #:parent [parent #f])
  (when val (unless (bytes? val)
              (raise-argument-error 'xbuffer-size "bytes" val)))
  (if (bytes? val)
      (bytes-length val)
      (resolve-length (xbuffer-len xb) val #:parent parent)))

(struct xbuffer xbase (len) #:transparent
  #:methods gen:xenomorphic
  [(define decode xbuffer-decode)
   (define encode xbuffer-encode)
   (define size xbuffer-size)])

(define (+xbuffer [len-arg #f]
                  #:length [len-kwarg #f])
  (define len (or len-arg len-kwarg #xffff))
  (unless (length-resolvable? len)
    (raise-argument-error '+xbuffer "resolvable length" len))
  (xbuffer len))