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

(define/post-decode (xoptional-decode xo [port-arg (current-input-port)] #:parent [parent #f])
  (define port (->input-port port-arg))
  (parameterize ([current-input-port port])
    (when (resolve-condition xo parent)
      (decode (xoptional-type xo) #:parent parent))))

(define/pre-encode (xoptional-encode xo val [port-arg (current-output-port)] #:parent [parent #f])
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (parameterize ([current-output-port port])
    (when (resolve-condition xo parent)
      (encode (xoptional-type xo) val #:parent parent))
    (unless port-arg (get-output-bytes port))))

(define/finalize-size (xoptional-size xo [val #f] #:parent [parent #f])
  (when (resolve-condition xo parent)
     (size (xoptional-type xo) val #:parent parent)))

(struct xoptional xbase (type condition) #:transparent
  #:methods gen:xenomorphic
  [(define decode xoptional-decode)
   (define encode xoptional-encode)
   (define size xoptional-size)])

(define (+xoptional type [condition #t])
  (xoptional type condition))
