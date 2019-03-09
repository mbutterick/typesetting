#lang racket/base
(require)
(provide (all-defined-out))

(struct Ptr (val [forceLarge #:auto]) #:transparent #:mutable #:auto-value #true
  ;; use prop:procedure instead of JS `valueOf`
  #:property prop:procedure (λ (ptr) (Ptr-val ptr)))