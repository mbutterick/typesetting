#lang fontkit/racket
(require racket/serialize)
(provide cloneDeep)

(define (cloneDeep val)
  (deserialize (serialize val)))

