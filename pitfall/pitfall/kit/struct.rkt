#lang br
(provide (struct-out String))

;; use structs to sub for missing node types

(struct String (string) #:transparent)