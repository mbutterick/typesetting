#lang racket
(provide cloneDeep)

(define (cloneDeep val)
  (parameterize ([print-graph #t])
    (read (open-input-string (~s val)))))