#lang racket/base
(provide (all-defined-out))
(struct Ptr (val [forceLarge #:auto]) #:transparent #:mutable #:auto-value #true)