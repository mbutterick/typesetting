#lang fontkit/racket
(require "font.rkt" "directory.rkt" "gpos.rkt")

(define f (openSync fira-path))
(define ds (send f _getTableStream 'GPOS))

(send GPOS decode ds)