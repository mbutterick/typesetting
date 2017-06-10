#lang racket/base
(require (for-syntax racket/base br/syntax))
(provide (for-syntax (all-from-out racket/base br/syntax)))
(provide (all-from-out racket/base) r+p)

(define-syntax-rule (r+p id ...) (begin (require id ...) (provide (all-from-out id ...))))

(r+p "helper.rkt"
     sugar/debug
     racket/class
     racket/list
     racket/string
     br/define
     sugar/define
     sugar/class
     sugar/js
     sugar/dict
     sugar/stub
     sugar/case)

(module reader syntax/module-reader
  #:language 'restructure/racket
  #:read @-read
  #:read-syntax @-read-syntax
  (require (prefix-in @- scribble/reader)))