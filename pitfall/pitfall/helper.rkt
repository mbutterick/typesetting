#lang racket/base
(require (for-syntax racket/base racket/syntax) racket/class sugar/list racket/list (only-in br/list push! pop!) racket/string racket/format)
(provide (all-defined-out) push! pop!)

(define-syntax (· stx)
  (syntax-case stx ()
    [(_ x ref)
     #'(cond
         [(object? x) (with-handlers ([exn:fail:object? (λ (exn) (send x ref))])
                        (get-field ref x))]
         [(hash? x) (hash-ref x 'ref)]
         [else (raise-argument-error '· (format "~a must be object or hash" 'x) x)])]
    [(_ x ref0 . refs) #'(· (· x ref0) . refs)]))

(define-syntax (·map stx)
  (syntax-case stx ()
    [(_ ref xs) #'(for/list ([x (in-list xs)]) (· x ref))]))

(define-syntax-rule (+= id thing) (begin (set! id (+ id thing)) id)) 
(define-syntax-rule (++ id) (+= id 1))
(define-syntax-rule (-- id) (+= id -1))
(define-syntax-rule (-= id thing) (+= id (- thing)))

(module+ test
  (require rackunit)
  (define C
    (class object%
      (super-new)
      (field [foo 'field])
      (define/public (bar) 'method)
      (define/public (zam) (hasheq 'zoom 'hash))))
  (define h (hasheq 'bam (new C) 'foo 'hashlet))
  (define o (new C))
  (check-equal? (· o foo) 'field)
  (check-equal? (· o bar) 'method)
  (check-equal? (· o zam zoom) 'hash)
  (check-equal? (· h bam foo) 'field)
  (check-equal? (· h bam bar) 'method)
  (check-equal? (· h bam zam zoom) 'hash)
  (check-equal? (·map foo (list o h)) '(field hashlet)))


(define (listify kvs)
  (for/list ([slice (in-list (slice-at kvs 2))])
    (cons (first slice) (second slice))))
(define-syntax-rule (define-hashifier id hasher) (define (id . kvs) (hasher (listify kvs))))
(define-hashifier mhash make-hash)
(define-hashifier mhasheq make-hasheq)
(define-hashifier mhasheqv make-hasheqv)

(module+ test
  (check-equal? (mhash 'k "v") (make-hash (list (cons 'k "v"))))) 


(define isBuffer? bytes?)
(define (newBuffer x) (string->bytes/latin-1 (format "~a" x)))
(define buffer-length bytes-length)


;; js-style `push`, which appends to end of list
(define-syntax-rule (push-end! id thing) (set! id (append id (list thing))))

(define-syntax-rule (push-field! field o expr) (set-field! field o (cons expr (get-field field o))))

(define-syntax-rule (push-end-field! field o expr)
  (set-field! field o (append (get-field field o) (list expr))))

(define-syntax-rule (pop-field! field o) (let ([xs (get-field field o)])
                                           (set-field! field o (cdr xs))
                                           (car xs)))
(define-syntax (increment-field! stx)
  (syntax-case stx ()
    [(_ field o)  #'(increment-field! field o 1)]
    [(_ field o expr)
     #'(begin (set-field! field o (+ (get-field field o) expr)) (get-field field o))]))

(define-syntax (increment! stx)
  (syntax-case stx ()
    [(_ id)  #'(increment! id 1)]
    [(_ id expr)
     #'(begin (set! id (+ id expr)) id)]))


(module+ test
  (define xs '(1 2 3))
  (push-end! xs 4)
  (check-equal? xs '(1 2 3 4)))

;; fancy number->string. bounds are checked, inexact integers are coerced.
(define (number x)
  (unless (and (number? x) (< -1e21 x 1e21))
    (raise-argument-error 'number "valid number" x))
  (let ([x (/ (round (* x 1e6)) 1e6)])
    (number->string (if (integer? x)
                        (inexact->exact x)
                        x))))

(module+ test
  (check-equal? (number 4.5) "4.5")
  (check-equal? (number 4.0) "4")
  (check-equal? (number 4) "4")
  (check-equal? (number -4) "-4"))


(define-syntax (send*/fold stx)
  (syntax-case stx ()
    [(_ o) #'o]
    [(_ o [m0 . args0] [m . args] ...)
     #'(send*/fold (send o m0 . args0) [m . args] ...)]))

(module+ test
  (define SFC (class object%
                (super-new)
                (field [sum 0])
                (define/public (add x) (set! sum (+ sum x)) this)))
  (define sfo (new SFC))
  (check-equal? (get-field sum (send*/fold sfo [add 1] [add 2] [add 3])) 6))

(define (bounded low x high)
  (if (high . < . low)
      (bounded high x low)
      (max low (min high x))))

(module+ test
  (check-equal? (bounded 0 2 1) 1)
  (check-equal? (bounded 1 2 0) 1)
  (check-equal? (bounded 0 -2 1) 0)
  (check-equal? (bounded 1 -2 0) 0)
  (check-equal? (bounded 0 .5 1) 0.5)
  (check-equal? (bounded 0 0 1) 0)
  (check-equal? (bounded 0 1 1) 1))


(struct exn:pitfall:test exn (data))

(define (raise-test-exn val)
  (raise (exn:pitfall:test "pitfall test exn" (current-continuation-marks) val)))

(define-syntax-rule (test-when cond expr)
  (if cond (raise-test-exn expr) expr))

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

(define-syntax (as-method stx)
  (syntax-case stx ()
    [(_ id) (with-syntax ([private-id (generate-temporary #'id)])
              #'(begin
                  (public [private-id id])
                  (define (private-id . args) (apply id this args))))]))

(define-syntax-rule (as-methods id ...)
  (begin (as-method id) ...))

(define (color-string? x)
  (and (string? x)
       (if (string-prefix? x "#")
           (or (= (string-length x) 4) (= (string-length x) 7))
           #t)))

(define-syntax-rule (define-subclass CLASS-ID (SUBCLASS-ID INIT-FIELD ...) . EXPRS)
  (define SUBCLASS-ID
    (class CLASS-ID
      (init-field INIT-FIELD ...) . EXPRS)))

(define (bytes->hex bstr)
  (map (λ (b) (string->symbol (string-append (if (< b 16)
                                                 "x0" "x") (~r b #:base 16)))) (bytes->list bstr)))

(module+ test
  (check-equal? (bytes->hex #"PNG") '(x50 x4e x47)))