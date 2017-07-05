#lang fontkit/racket
(require fontkit "subset.rkt" rackunit xenomorph racket/serialize)

(define fira-path "../pitfall/test/assets/fira.ttf")
(define f (openSync fira-path))
(define gpos (· f GPOS))
(define ttfs (make-object TTFSubset f))
(send ttfs encodeStream)

#;(define gsub (· f GSUB))

#;(get (· gpos lookupList) 7)

