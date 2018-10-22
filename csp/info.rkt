#lang info
(define collection 'multi)
(define deps '(("base" #:version "6.0") "sugar" "rackunit-lib" "debug" "graph"))
(define update-implies '("sugar"))
(define scribblings '(("csp/scribblings/csp.scrbl" (multi-page))))
;(define compile-omit-paths '("tests" "raco.rkt"))