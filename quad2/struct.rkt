#lang racket/base
(provide (all-defined-out))

(struct $drawing-inst () #:transparent)
(struct $move $drawing-inst (posn) #:transparent) ; an absolute location in coordinate system (not relative to last loc)
(struct $text $drawing-inst (charint) #:transparent)
(struct $doc $drawing-inst (inst) #:transparent)
(struct $page $drawing-inst (inst) #:transparent)

(struct attr (name) #:transparent)
(struct attr-uncased-string attr () #:transparent)
(struct attr-cased-string attr () #:transparent)
(struct attr-dimension-string attr () #:transparent)
(struct attr-path attr () #:transparent)
(struct attr-numeric attr () #:transparent)
(struct attr-boolean attr () #:transparent)