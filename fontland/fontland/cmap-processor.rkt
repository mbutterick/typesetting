#lang fontkit/racket
(provide CmapProcessor)

(define-subclass object% (CmapProcessor cmapTable)
  (super-new))