#lang debug racket

(provide (all-defined-out))

(struct ttf-font (port decoded-tables src directory ft-face hb-font hb-buf crc get-head-table-proc)
  #:transparent #:mutable)

(define (ft-face this) (or (force (ttf-font-ft-face this)) (error 'ft-face-not-available)))
