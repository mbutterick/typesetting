#lang racket/base
(require (for-syntax racket/base br/syntax) br/define)
(provide (for-syntax (all-from-out racket/base br/syntax)))
(provide (all-from-out racket/base) r+p)

(define-macro (r+p ID ...)
  #'(begin (require ID ...) (provide (all-from-out ID ...))))

(r+p "helper.rkt"
     "generic.rkt"
     sugar/debug
     racket/class
     racket/list
     racket/string
     racket/function
     br/define
     sugar/define
     sugar/class
     sugar/js
     sugar/dict
     sugar/stub
     sugar/port
     sugar/case)


(module reader syntax/module-reader
  #:language 'restructure/racket
  #:read @-read
  #:read-syntax @-read-syntax
  (require (prefix-in @- scribble/reader)))