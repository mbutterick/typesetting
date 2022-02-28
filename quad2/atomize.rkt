#lang debug racket/base
(require racket/match
         "compile.rkt"
         "quad.rkt")
(provide atomize)

#|

My idea here is that instead of comparing attrs by using eq? with a hash,
we can use eq? (not equal?) on an association list
the idea being that if only update the attrs by consing onto the front,
then the process is strictly accretive, that is:
we are never allocating a fresh list to hold existing values
For instance, the top-level attrs represent a list object
that will eventually be the tail of the attrs in every atomized quad.
|#

(define-pass (atomize q)
  #:pre quad?
  #:post (list-of quad?)
  (match (quad-elems q)
    [(cons str _)
     (for/list ([c (in-string str)])
               (make-quad #:attrs (make-quad-attrs (list (cons 'char c)))))]
    [_ (error 'atomize-branch-unimplemented)]))