#lang pitfall/racket
(provide CmapProcessor)

(define-subclass object% (CmapProcessor cmapTable)
  (super-new))