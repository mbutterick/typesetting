#lang info
(define collection 'multi)
(define deps '("at-exp-lib"
               "beautiful-racket-lib"
               "brag"
               "describe"
               "png-image"
               "srfi-lite-lib"
               "sugar"
               "debug"
               "base" 
               "draw-lib"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define version "0.0")
(define pkg-authors '(mb))
(define test-omit-paths 'all)
#;(define compile-omit-paths '("pdfkit" "fontkit"))
