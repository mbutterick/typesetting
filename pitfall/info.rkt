#lang info
(define collection 'multi)
(define deps '("at-exp-lib"
               "beautiful-racket-lib"
               "brag"
               "describe"
               "png-image"
               "srfi-lite-lib"
               "sugar"
               "base" "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define version "0.0")
(define pkg-authors '(mb))
(define compile-omit-paths '("pdfkit" "fontkit"))