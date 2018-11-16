#lang racket/base
(require "racket.rkt")

(provide CmapProcessor)

(define-subclass object% (CmapProcessor cmapTable)
  (super-new))