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
     sugar/unstable/class
     sugar/unstable/js
     sugar/unstable/dict
     sugar/unstable/stub
     sugar/unstable/port
     sugar/unstable/case)

(provide define-procedures)
(define-macro (define-procedures (NEW ...) (OLD ...))
  #'(define-values (NEW ...)
      (values (if (procedure? OLD)
                  (procedure-rename OLD 'NEW)
                  OLD) ...)))

(module reader syntax/module-reader
  #:language 'xenomorph/private/racket)