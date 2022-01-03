#lang info
(define collection 'multi)
(define version "0.0")
(define deps '("crc32c"
               "db-lib"
               ["base" #:version "7.1"]
               "beautiful-racket-lib"
               "debug"
               "draw-lib"
               "rackunit-lib"
               "png-image"
               "sugar"
               "xenomorph"))
(define update-implies '("xenomorph"))