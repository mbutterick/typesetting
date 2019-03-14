#lang debug racket/base
(require racket/struct racket/match)
(provide (all-defined-out))

(struct deque (start length) #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (d) 'deque)
      (λ (d) (deque->list d))))])
(struct deque-item (val prev next) #:mutable)

(define (before! di1 di2)
  (set-deque-item-next! di1 di2)
  (set-deque-item-prev! di2 di1))

(define (insert-before! new-di di)
  ((deque-item-prev di) . before! . new-di)
  (new-di . before! . di))

(define (remove! di)
  ((deque-item-prev di) . before! . (deque-item-next di)))

(define (push-end! d . vals)
  (apply push-start! d #:end #true vals))

(define (push-start! d #:end [end? #f] . vals)
  (define-values (val-count first-di)
    (for/fold ([count 0]
               [first-di #f])
              ([val (in-list vals)])
      (define di (deque-item val #f #f))
      (match (deque-start d)
        [#false (set-deque-start! d di)
                (before! di di)]
        [start (di . insert-before! . start)])
      (values (add1 count) (or first-di di))))
  (unless (zero? val-count)
    (unless end?
      (set-deque-start! d first-di))
    (set-deque-length! d (+ val-count (deque-length d)))))

(define (pop-start! d #:end [end? #f])
  (unless (zero? (deque-length d))
    (define popdi ((if end? deque-item-prev values) (deque-start d)))
    (begin0
      (deque-item-val popdi)
      (set-deque-start! d (and (> (deque-length d) 1) (deque-item-next popdi)))
      (remove! popdi)
      (set-deque-length! d (sub1 (deque-length d))))))

(define (pop-end! d)
  (pop-start! d #:end #true))

(define (make-deque . vals)
  (define d (deque #f 0))
  (apply push-end! d vals)
  d)

(define (deque-ref d idx)
  (unless (< idx (deque-length d))
    (error 'deque-idx-too-large))
  (for/fold ([di (deque-start d)]
             #:result (deque-item-val di))
            ([i (in-range idx)])
    (deque-item-next di)))

(define (deque-rotate! d [count 0])
  (unless (or (zero? count) (< (deque-length d) 2))
    (cond
      [(< (abs count) (deque-length d))
       (define opp-count ((if (positive? count) - +) (- (deque-length d) (abs count))))
       (define new-count (if (< (abs opp-count) (abs count)) opp-count count))
       (define dir (if (positive? new-count) deque-item-next deque-item-prev))
       (for/fold ([di (deque-start d)]
                  #:result (set-deque-start! d di))
                 ([i (in-range (abs new-count))])
         (dir di))]
      [else (deque-rotate! d (modulo count ((if (positive? count) + -) (deque-length d))))])))

(define (deque->list d)
  (for/fold ([vals null]
             [di (deque-start d)]
             #:result (reverse vals))
            ([i (in-range (deque-length d))])
    (values (cons (deque-item-val di) vals) (deque-item-next di))))

(define (list->deque xs)
  (apply make-deque xs))

(module+ test
  (require rackunit)
  (define d (make-deque 42))
  (check-equal? (deque-length d) 1)
  (push-end! d 43 44 45)
  (check-equal? (deque-length d) 4)
  (check-equal? (deque-ref d 0) 42)
  (check-equal? (deque-ref d 1) 43)
  (check-equal? (deque-ref d 2) 44)
  (check-equal? (deque-ref d 3) 45)
  (push-start! d 39 40 41)
  (check-equal? (deque-length d) 7)
  (check-equal? (deque-ref d 0) 39)
  (check-equal? (deque-ref d 1) 40)
  (check-equal? (deque-ref d 2) 41)
  (check-equal? (deque-ref d 3) 42)
  (check-equal? (pop-start! d) 39)
  (check-equal? (pop-start! d) 40)
  (check-equal? (pop-end! d) 45)
  (check-equal? (pop-end! d) 44)
  (check-equal? (deque-length d) 3)
  (check-equal? (deque->list d) '(41 42 43))
  (deque-rotate! d 1)
  (check-equal? (deque->list d) '(42 43 41))
  (deque-rotate! d -1)
  (check-equal? (deque->list d) '(41 42 43))
  (deque-rotate! d (deque-length d))
  (check-equal? (deque->list d) '(41 42 43))
  (let ([d (make-deque 90)])
    (pop-start! d)
    (push-end! d -1)
    (check-equal? (deque->list d) (list -1))))
