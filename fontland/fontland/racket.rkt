#lang racket/base
(require (for-syntax racket/base br/syntax))
(provide (for-syntax (all-from-out racket/base br/syntax)))
(provide (all-from-out racket/base) r+p (all-defined-out))

(define-syntax-rule (r+p id ...) (begin (require id ...) (provide (all-from-out id ...))))

(r+p "helper.rkt"
     sugar/debug
     racket/class
     racket/file
     racket/match
     racket/string
     racket/format
     racket/contract
     racket/dict
     racket/list
     racket/port
     racket/function
     br/define
     sugar/class
     sugar/js
     sugar/dict
     sugar/stub
     sugar/port
     sugar/contract
     describe)


(module reader syntax/module-reader
  #:language 'fontkit/racket
  #:read @-read
  #:read-syntax @-read-syntax
  (require (prefix-in @- scribble/reader)))