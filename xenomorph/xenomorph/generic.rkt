#lang racket/base
(require racket/generic)
(provide (all-defined-out))

(define-generics xenomorphic
  (encode xenomorphic val [port] #:parent [parent])
  (decode xenomorphic [port] #:parent [parent])
  (size xenomorphic [item] #:parent [parent]))