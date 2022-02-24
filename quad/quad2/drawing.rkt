#lang racket/base
(provide (all-defined-out))

(struct $drawing-inst () #:transparent)
(struct $move $drawing-inst (posn) #:transparent) ; an absolute location in coordinate system (not relative to last loc)
(struct $text $drawing-inst (char) #:transparent)
(struct $doc $drawing-inst (inst) #:transparent)
(struct $page $drawing-inst (inst) #:transparent)