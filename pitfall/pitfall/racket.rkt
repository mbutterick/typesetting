#lang racket/base
(provide (all-from-out racket/base))

(define-syntax-rule (r+p id ...) (begin (require id ...) (provide (all-from-out id ...))))

(r+p "helper.rkt"
     "params.rkt"
     "struct.rkt"
     sugar/debug
     racket/class
     racket/match
     racket/string
     racket/format
     racket/contract
     racket/list
     racket/port)

(module reader syntax/module-reader
  #:language "racket.rkt"
  #:read @-read
  #:read-syntax @-read-syntax
  (require (prefix-in @- scribble/reader)))