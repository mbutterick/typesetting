#lang debug racket

(provide (all-defined-out))

(struct TTFFont (_port _decoded-tables _src _directory _ft-face _hb-font _hb-buf _crc _get-head-table)
  #:transparent #:mutable)
