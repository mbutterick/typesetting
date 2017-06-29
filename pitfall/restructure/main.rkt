#lang restructure/racket

(r+p "base.rkt"
     "number.rkt"
     "struct.rkt"
     "versioned-struct.rkt"
     "string.rkt"
     "array.rkt"
     "lazy-array.rkt"
     "bitfield.rkt"
     "stream.rkt"
     "buffer.rkt"
     "pointer.rkt")

(test-module
 (require "number-test.rkt"
          "struct-test.rkt"
          "versioned-struct-test.rkt"
          "string-test.rkt"
          "array-test.rkt"
          "lazy-array-test.rkt"
          "bitfield-test.rkt"
          "stream-test.rkt"
          "buffer-test.rkt"
          #;"pointer-test.rkt"))