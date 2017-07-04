#lang fontkit/racket
(require fontkit rackunit xenomorph racket/serialize)

(define fira-path "../pitfall/test/assets/fira.ttf")
(define f (openSync fira-path))
(define gpos (· f GPOS))
(define gsub (· f GSUB))

(send (· gpos lookupList) get 1)
