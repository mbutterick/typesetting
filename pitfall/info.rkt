#lang info
(define collection 'multi)
(define deps '("base" "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define version "0.0")
(define pkg-authors '(mb))
(define compile-omit-paths '("pitfall/test/node_modules/pdfkit"))