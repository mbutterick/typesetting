#lang racket/base
(require "racket.rkt")

(require racket/serialize)
(provide cloneDeep)

(define (cloneDeep val)
  (deserialize (serialize val)))

