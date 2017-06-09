#lang racket/base
(require (for-syntax racket/base br/syntax) br/define)
(provide (all-defined-out))

(begin-for-syntax
  (require racket/string racket/format)
  (define (make-prefix caller-stx)
    (string-join (map ~a (list (syntax-source caller-stx) (syntax-line caller-stx))) ":" #:after-last ":")))

(define-macro (define-stub-stop ID)
  (with-pattern ([ERROR-ID (suffix-id (prefix-id (make-prefix caller-stx) #'ID) ":not-implemented")])
                #'(define (ID . args)
                    (error 'ERROR-ID))))

(provide (rename-out [define-stub-stop define-stub]))

(define-macro (define-stub-go ID)
  (with-pattern ([ERROR-ID (suffix-id (prefix-id (make-prefix caller-stx) #'ID) ":not-implemented")])
                #'(define (ID . args)
                    (displayln 'ERROR-ID))))

(define-macro (define-unfinished (ID . ARGS) . BODY)
  (with-pattern ([ID-UNFINISHED (suffix-id (prefix-id (make-prefix caller-stx) #'ID) ":unfinished")])
                #'(define (ID . ARGS)
                    (begin . BODY)
                    (error 'ID-UNFINISHED))))


(define-macro (unfinished)
  (with-pattern ([ID-UNFINISHED (prefix-id (syntax-source caller-stx) ":" (syntax-line caller-stx) ":" #'unfinished)])
                #'(error 'ID-UNFINISHED)))