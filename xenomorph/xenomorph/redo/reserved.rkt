#lang racket/base
(require "helper.rkt" "util.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Reserved.coffee
|#

(define (xreserved-decode xo [port-arg (current-input-port)] #:parent [parent #f])
  (define port (->input-port port-arg))
  (pos port (+ (pos port) (size xo #f #:parent parent)))
  (void))

(define (xreserved-encode xo val [port-arg (current-output-port)] #:parent [parent #f])
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (write-bytes (make-bytes (size xo val #:parent parent) 0) port)
  (unless port-arg (get-output-bytes port)))

(define (xreserved-size xo [val #f] #:parent [parent #f])
  (define item-size (size (xreserved-type xo)))
  (define count (resolve-length (xreserved-count xo) #f #:parent parent))
  (finalize-size (* item-size count)))

(struct xreserved (type count) #:transparent
  #:methods gen:xenomorphic
  [(define decode xreserved-decode)
   (define encode xreserved-encode)
   (define size xreserved-size)])

(define (+xreserved type [count 1])
  (xreserved type count))