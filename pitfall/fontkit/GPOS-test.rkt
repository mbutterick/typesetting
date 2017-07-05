#lang fontkit/racket
(require fontkit "subset.rkt" rackunit xenomorph racket/serialize)

(define fira-path "../pitfall/test/assets/fira.ttf")
(define f (openSync fira-path))
(define gpos (· f GPOS))
(get (· gpos lookupList) 11)

