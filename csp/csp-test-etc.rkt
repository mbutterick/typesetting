#lang at-exp racket
(require "csp.rkt" racket/port rackunit)

(use-mrv? #f)
(use-reduce-arity? #f)
(use-mac? #f)
(use-remove-constraints? #f)
(use-validate-assignments? #t)

(define (neq? x y) (not (eq? x y)))

(define c (make-csp))
(add-vars! c '(wa nsw t q nt v sa) '(red green blue))
(add-constraint! c neq? '(wa nt))
(add-constraint! c neq? '(nt q))
(add-constraint! c neq? '(q nsw))
(add-constraint! c neq? '(nsw v))
(add-constraint! c neq? '(sa wa))
(add-constraint! c neq? '(sa nt))
(add-constraint! c neq? '(sa q))
(add-constraint! c neq? '(sa nsw))
(add-constraint! c neq? '(sa v))

(solve c)