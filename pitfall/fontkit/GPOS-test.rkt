#lang fontkit/racket
(require fontkit "subset.rkt" rackunit xenomorph racket/serialize)

(define fira-path "../pitfall/test/assets/fira.ttf")
(define f (openSync charter-path))
(define ttfs (make-object TTFSubset f))
(send ttfs encodeStream)

#;(define gpos (· f GPOS))
#;(get (· gpos lookupList) 7)

