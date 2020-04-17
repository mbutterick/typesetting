#lang info
(define collection 'multi)
(define version "0.0")
(define test-omit-paths '("ptest"))
(define deps '("draw-lib"
               "with-cache"
               "at-exp-lib"
               ["base" #:version "7.1"]
               "beautiful-racket-lib"
               "brag"
               "fontland"
               "rackunit-lib"
               "srfi-lite-lib"
               "sugar"
               "gregor"))
(define build-deps '("debug"))
(define update-implies '("fontland"))
