#lang info
(define collection 'multi)
(define deps '("beautiful-racket-lib"
               "htdp-lib"
               "math-lib"
               ("base" #:version "6.0") "sugar" "rackunit-lib" "debug" "graph"))
(define update-implies '("sugar"))(define build-deps '("at-exp-lib"
                     "math-doc"
                     "racket-doc"
                     "scribble-lib"))
