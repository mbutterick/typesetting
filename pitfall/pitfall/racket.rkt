#lang racket/base
(provide (all-from-out racket/base) r+p)

(define-syntax-rule (r+p id ...) (begin (require id ...) (provide (all-from-out id ...))))

(r+p "helper.rkt"
     "param.rkt"
     "struct.rkt"
     sugar/debug
     racket/class
     racket/file
     racket/match
     racket/string
     racket/format
     racket/contract
     racket/list
     racket/port
     racket/function)

(module reader syntax/module-reader
  #:language 'pitfall/racket
  #:read @-read
  #:read-syntax @-read-syntax
  (require (prefix-in @- scribble/reader)))