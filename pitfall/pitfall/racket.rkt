#lang racket/base
(require (for-syntax racket/base br/syntax))
(provide (for-syntax (all-from-out racket/base br/syntax)))
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
     racket/function
     br/define
     sugar/unstable/class
     sugar/unstable/js
     sugar/unstable/dict
     sugar/unstable/stub
     sugar/unstable/port
     sugar/unstable/contract
     describe)

(module reader syntax/module-reader
  #:language 'pitfall/racket
  #:read @-read
  #:read-syntax @-read-syntax
  (require (prefix-in @- scribble/reader)))