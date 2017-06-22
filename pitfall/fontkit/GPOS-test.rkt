#lang fontkit/racket
(require fontkit rackunit restructure)

(define fira-path "../pitfall/test/assets/fira.ttf")
(define f (openSync fira-path))
(report 'start-decode)
(define gpos-hash (send GPOS decode (send f _getTableStream 'GPOS)))
gpos-hash
