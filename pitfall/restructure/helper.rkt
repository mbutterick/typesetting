#lang racket/base
(require (for-syntax racket/base br/syntax) racket/class br/define)
(provide (all-defined-out))

(define (read-bytes-exact count p)
  (define bs (read-bytes count p))
  (unless (and (bytes? bs) (= (bytes-length bs) count))
    (raise-argument-error 'read-bytes-exact (format "byte string length ~a" count) bs))
  bs)

(define RBase
  (class object%
    (super-new)    
    (abstract decode)
    (abstract encode)
    #;(abstract size)))


(define-macro (define-subclass SUPERCLASS (ID . INIT-ARGS) . BODY)
  #'(define ID (class SUPERCLASS (super-new) (init-field . INIT-ARGS) . BODY)))

(require (for-syntax sugar/debug))
(define-macro (getter-field [ID . EXPRS])
  (with-pattern ([_ID (prefix-id "_" #'ID)])
    #`(begin
        (field [(ID _ID)  . EXPRS])
        (public (_ID ID))
        (#,(if (syntax-property caller-stx 'override) #'define/override #'define) (_ID) ID))))

(define-macro (getter-field/override [ID . EXPRS])
  (syntax-property #'(getter-field [ID . EXPRS]) 'override #t))

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

(define-macro (define+provide ID . EXPRS)
  #'(begin
      (provide ID)
      (define ID . EXPRS)))

(require sugar/list)
(define (listify kvs)
  (for/list ([slice (in-list (slice-at kvs 2))])
    (cons (car slice) (cadr slice))))
(define-syntax-rule (define-hashifier id hasher) (define (id . kvs) (hasher (listify kvs))))
(define-hashifier mhash make-hash)
(define-hashifier mhasheq make-hasheq)
(define-hashifier mhasheqv make-hasheqv)

(provide dictify)
(define (dictify . xs) (listify xs))

(define (port-position port)
  (define-values (l c p) (port-next-location port))
  p)

(define-syntax (· stx)
  (syntax-case stx ()
    [(_ x ref)
     #'(cond
         [(object? x) (with-handlers ([exn:fail:object? (λ (exn) (send x ref))])
                        (get-field ref x))]
         [(hash? x) (hash-ref x 'ref #f)]
         [else (raise-argument-error '· (format "~a must be object or hash" 'x) x)])]
    [(_ x ref0 . refs) #'(· (· x ref0) . refs)]))

(define-macro (define-case-macro ID PRED)
  #'(define-macro-cases ID
      [(_ TEST-VAL [(MATCH0 . MATCH-VALS) . RESULT] (... ...) [else . ELSE-RESULT])
       #'(cond
           [(PRED TEST-VAL '(MATCH0 . MATCH-VALS)) . RESULT] (... ...)
           [else . ELSE-RESULT])]
      [(_ TEST-VAL MATCH-CLAUSE (... ...))
       #'(ID TEST-VAL
             MATCH-CLAUSE (... ...)
             [else (error 'ID (format "no match for ~a" TEST-VAL))])]))

;; like case but strictly uses `eq?` comparison (as opposed to `equal?`)
(define-case-macro caseq memq)