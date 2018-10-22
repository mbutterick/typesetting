#lang racket/base
(require racket/class racket/list (for-syntax racket/base racket/syntax))
(provide (all-defined-out))
(require rackunit)

(define-syntax-rule (forever expr ...)
  (for ([i (in-naturals)])
    expr ...))

(define-syntax-rule (forever/until expr ...)
  (for/or ([i (in-naturals)])
    expr ...))

(define-syntax-rule (for-each-send proc objects)
  (for-each (λ(o) (send o proc)) objects))

(define-syntax-rule (make-proc<%> proc-name)
  (interface* ()
              ([prop:procedure
                (λ(this . args)
                  (send/apply this proc-name args))])
              proc-name))

(define-simple-check (check-hash-items h1 h2)
  (for/and ([(k1 v1) (in-hash h1)])
    (equal? (hash-ref h2 k1) v1)))

(define (list-comparator xs ys)
  ;; For use in sort. Compares two lists element by element.
  (cond
    [(equal? xs ys) #f] ; elements are same, so no sort preference    
    [(and (null? xs) (not (null? ys))) #t] ; ys is longer, so #t 
    [(and (not (null? xs)) (null? ys)) #f] ; xs is longer, so #f makes it sort later
    [else (let ([x (car xs)][y (car ys)])
            (cond
              [(equal? x y) (list-comparator (cdr xs) (cdr ys))]
              [(and (real? x) (real? y)) (< x y)]
              [(and (symbol? x) (symbol? y)) (apply string<? (map symbol->string (list x y)))] 
              [(and (string? x) (string? y)) (string<? x y)]
              [else (error 'list-comparator (format "Can’t compare ~v and ~v" x y))]))]))

(module+ test 
  (check-false (list-comparator null null))
  (check-false (list-comparator (range 2) (range 2)))
  (check-true (list-comparator (range 2) (range 4)))
  (check-false (list-comparator (range 4) (range 2)))
  (check-true (list-comparator '(1 1 "a") '(1 1 "b")))
  (check-true (list-comparator '(1 1 a) '(1 1 b))))

(define-syntax-rule (car-pop! xs)
  (let ([i (car xs)])
      (set! xs (cdr xs))
      i))

(define-syntax-rule (py-pop! xs)
  (let ([i (last xs)])
      (set! xs (drop-right xs 1))
      i))

(module+ test
  (let ([xs '(1 2 3)])
    (check-equal? (py-pop! xs) 3)
    (check-equal? xs '(1 2))))

(define-syntax-rule (py-append! xs x)
  (set! xs `(,@xs ,x)))

(define-syntax-rule (py-extend! xs x)
  (set! xs `(,@xs ,@x)))

(module+ test
  (let ([xs '(1 2 3)])
    (py-append! xs (range 2))
    (check-equal? xs '(1 2 3 (0 1))))
  (let ([xs '(1 2 3)])
    (py-extend! xs (range 2))
    (check-equal? xs '(1 2 3 0 1))))


(define (word-value . xs)
  (let ([xs (reverse xs)])
    (for/sum ([i (in-range (length xs))])
      (* (list-ref xs i) (expt 10 i)))))