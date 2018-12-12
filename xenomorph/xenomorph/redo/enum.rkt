#lang racket/base
(require "helper.rkt" racket/list)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Enum.coffee
|#

(define (xenum-decode xe [port-arg (current-input-port)] #:parent [parent #f])
  (define port (->input-port port-arg))
  (parameterize ([current-input-port port])
  (define index (decode (xenum-type xe)))
  (or (list-ref (xenum-options xe) index) index)))

(define (xenum-size xe [val #f] [parent #f]) (size (xenum-type xe)))

(define (xenum-encode xe val [port-arg (current-output-port)] #:parent [parent #f])
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (parameterize ([current-output-port port])
  (define index (index-of (xenum-options xe) val))
  (unless index
    (raise-argument-error 'xenum-encode "valid option" val))
  (encode (xenum-type xe) index)
  (unless port-arg (get-output-bytes port))))

(struct xenum (type options) #:transparent
  #:methods gen:xenomorphic
  [(define decode xenum-decode)
   (define encode xenum-encode)
   (define size xenum-size)])

(define (+xenum type [options null])
  (xenum type options))