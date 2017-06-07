#lang racket/base
(require (for-syntax racket/base br/syntax) racket/class br/define)
(provide (all-defined-out))

(define (read-bytes-exact count p)
  (define bs (read-bytes count p))
  (unless (and (bytes? bs) (= (bytes-length bs) count))
    (raise-argument-error 'read-bytes-exact (format "byte string length ~a" count) bs))
  bs)

(define BinaryIO%
  (class object%
    (super-new)    
    (abstract decode)
    (abstract encode)
    (abstract size)))


(define-macro (define-subclass SUPERCLASS (ID . INIT-ARGS) . BODY)
  #'(define ID (class SUPERCLASS (super-new) (init-field . INIT-ARGS) . BODY)))

(define-macro (getter-field [ID . EXPRS])
  (with-pattern ([_ID (prefix-id "_" #'ID)])
    #'(begin
        (field [(ID _ID)  . EXPRS])
        (public (_ID ID))
        (define (_ID) ID))))

(define-macro (test-module . EXPRS)
  #`(module+ test
      (require #,(datum->syntax caller-stx 'rackunit))
      . EXPRS))

(define-macro (define-stub-stop ID)
  (with-pattern ([ERROR-ID (suffix-id (prefix-id (syntax-source #'this) ":" #'ID) ":not-implemented")])
                #'(define (ID . args)
                    (error 'ERROR-ID))))

(provide (rename-out [define-stub-stop define-stub]))

(define-macro (define-stub-go ID)
  (with-pattern ([ERROR-ID (suffix-id (prefix-id (syntax-source #'this) ":" #'ID) ":not-implemented")])
                #'(define (ID . args)
                    (displayln 'ERROR-ID))))

(define-macro (define-unfinished (ID . ARGS) . BODY)
  (with-pattern ([ID-UNFINISHED (suffix-id (prefix-id (syntax-source #'this) ":" #'ID) ":unfinished")])
                #'(define (ID . ARGS)
                    (begin . BODY)
                    (error 'ID-UNFINISHED))))


(define-macro (unfinished)
  (with-pattern ([ID-UNFINISHED (prefix-id (syntax-source caller-stx) ":" (syntax-line caller-stx) ":" #'unfinished)])
                #'(error 'ID-UNFINISHED)))