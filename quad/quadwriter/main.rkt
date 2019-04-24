#lang debug racket/base
(require pollen/tag "lang-helper.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin))

(define (doc-proc strs) (apply q strs))
(make-module-begin doc-proc)

(module reader racket/base
  (require "lang-helper.rkt")
  (provide (rename-out [rs read-syntax]) get-info)
  (define rs (make-read-syntax 'quadwriter
                               (λ (path ip)
                                 (for/list ([tok (in-port read ip)])
                                   tok)))))