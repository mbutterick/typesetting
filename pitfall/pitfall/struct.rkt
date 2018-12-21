#lang racket/base
(provide (all-defined-out))

;; use structs to sub for missing node types

(struct String (string) #:transparent)

;; for JPEG and PNG
(struct image (label width height obj) #:transparent #:mutable)