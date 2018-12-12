#lang racket/base
(require "helper.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Optional.coffee
|#

(define (resolve-condition xo parent)
  (define maybe-proc (xoptional-condition xo))
  (if (procedure? maybe-proc)
      (maybe-proc parent)
      maybe-proc))

(define (xoptional-decode xo [port-arg (current-input-port)] #:parent [parent #f])
  (define port (->input-port port-arg))
  (parameterize ([current-input-port port])
  (when (resolve-condition xo parent)
    (decode (xoptional-type xo) #:parent parent))))

(define (xoptional-encode xo val [port-arg (current-output-port)] #:parent [parent #f])
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (parameterize ([current-output-port port])
  (when (resolve-condition xo parent)
    (encode (xoptional-type xo) val #:parent parent))
  (unless port-arg (get-output-bytes port))))

(define (xoptional-size xo [val #f] #:parent [parent #f])
  (if (resolve-condition xo parent)
      (size (xoptional-type xo) val #:parent parent)
      0))

(struct xoptional (type condition) #:transparent
  #:methods gen:xenomorphic
  [(define decode xoptional-decode)
   (define encode xoptional-encode)
   (define size xoptional-size)])

(define (+xoptional type [condition #t])
  (xoptional type condition))
