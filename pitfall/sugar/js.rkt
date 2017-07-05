#lang racket/base
(require racket/class (for-syntax racket/base racket/syntax br/syntax) br/define racket/dict)
(provide (all-defined-out))


;; js-style `push`, which appends to end of list
(define-macro (push-end! ID THING)
  #'(set! ID (append ID (list THING))))


(define-macro-cases increment!
  [(_ ID)  #'(increment! ID 1)]
  [(_ ID EXPR)
   #'(begin (set! ID (+ ID EXPR)) ID)])

(module+ test
  (define xs '(1 2 3))
  (push-end! xs 4)
  (check-equal? xs '(1 2 3 4)))

(define-macro (+= ID THING) #'(begin (set! ID (+ ID THING)) ID)) 
(define-macro (++ ID) #'(+= ID 1))
(define-macro (-- ID) #'(+= ID -1))
(define-macro (-= ID THING) #'(+= ID (- THING)))


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


(define ·-helper
  (procedure-rename
   (λ (x . refs)
     (for/fold ([x x])
               ([ref (in-list refs)])
       (cond
         ;; dict first, to catch objects that implement gen:dict
         [(dict? x) (dict-ref x ref #f)]
         ;; give `send` precedence (presence of method => wants runtime resolution of value)
         [(object? x) (cond
                        [(memq ref (interface->method-names (object-interface x))) (dynamic-send x ref)]
                        [(memq ref (field-names x)) (dynamic-get-field ref x)]
                        [else #f])]
         [else (raise-argument-error '· "object or dict" x)]))) '·))

(define-macro (· X REF ...) #'(·-helper X 'REF ...))

#;(module+ test
    (define c (class object%
                (super-new)
                (field [a 42])
                (define/public (res) (hash 'res (hash 'b 43)))))
    (define co (make-object c))
    (define h2 (hash 'a 42 'res co))
    (check-equal? (· h2 a) 42)
    (check-equal? (· h2 b) 43)
    (check-equal? (· co a) 42)
    (check-equal? (· co b) 43))

(define-macro (·map REF XS)
  #'(for/list ([x (in-list XS)]) (· x REF)))

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

