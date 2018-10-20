#lang debug racket
(require sugar "hacs.rkt")

(current-inference forward-check)
(current-select-variable mrv)
(current-order-values shuffle)
(current-shuffle #true)