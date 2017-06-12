#lang racket/base
(require (for-syntax racket/base racket/syntax br/syntax) br/define racket/class)
(provide (all-defined-out))

(define string%
  (class* object% (writable<%>)
    (super-new)
    (init-field [data #f])
    (define (get-string)
      (with-handlers ([exn:fail:object? (λ (exn) data)])
        (send this toString)))
    (define/public (custom-write port) (write (get-string) port))
    (define/public (custom-display port) (display (get-string) port))))

(define mixin-tester%
  (class object%
    (super-new)
    (define/public (addContent val) (make-object string% val))))

(define-macro (as-method ID)
  (with-pattern ([PRIVATE-ID (generate-temporary #'ID)])
    #'(begin
        (public [PRIVATE-ID ID])
        (define (PRIVATE-ID . args) (apply ID this args)))))


(define-macro (as-methods ID ...)
  #'(begin (as-method ID) ...))


(define-macro (define-subclass SUPERCLASS (ID . INIT-ARGS) . BODY)
  #'(define-subclass* SUPERCLASS (ID . INIT-ARGS) (super-new) . BODY))


(define-macro (define-subclass* SUPERCLASS (ID . INIT-ARGS) . BODY)
  (with-pattern ([+ID (prefix-id "+" #'ID)]
                 [ID? (suffix-id #'ID "?")])
    #'(begin
        (define ID (class SUPERCLASS (init-field . INIT-ARGS) . BODY))
        (define (ID? x) (is-a? x ID))
        (define (+ID . args) (apply make-object ID args)))))


(define-macro (push-field! FIELD O EXPR)
  #'(set-field! FIELD O (cons EXPR (get-field FIELD O))))


(define-macro (push-end-field! FIELD O EXPR)
  #'(set-field! FIELD O (append (get-field FIELD O) (list EXPR))))

(define-macro (pop-field! FIELD O)
  #'(let ([xs (get-field FIELD O)])
      (set-field! FIELD O (cdr xs))
      (car xs)))

(define-macro-cases increment-field!
  [(_ FIELD O)  #'(increment-field! FIELD O 1)]
  [(_ FIELD O EXPR)
   #'(begin (set-field! FIELD O (+ (get-field FIELD O) EXPR)) (get-field FIELD O))])


(define-macro (getter-field/override [ID . EXPRS])
  (syntax-property #'(getter-field [ID . EXPRS]) 'override #t))


(define-macro (getter-field [ID . EXPRS])
  (with-pattern ([_ID (prefix-id "_" #'ID)])
    #`(begin
        (field [(ID _ID)  . EXPRS])
        (public (_ID ID))
        (#,(if (syntax-property caller-stx 'override) #'define/override #'define) (_ID) ID))))