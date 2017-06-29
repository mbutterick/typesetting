#lang racket/base

(require racket/generic
         (prefix-in b: racket/base)
         racket/dict
         racket/class
         racket/match)

(provide (all-defined-out))

(define-generics indexable
  (ref indexable i)
  (ref-set! indexable i v)
  (ref-keys indexable)
  #:defaults
  ([hash? (define ref hash-ref)
          (define ref-set! hash-set!)
          (define ref-keys hash-keys)]
   [object? (define (ref o i) (with-handlers ([exn:fail:object? (λ (exn) (hash-ref (get-field _hash o) i))]) (dynamic-get-field i o)))
            (define (ref-set! o i v) (with-handlers ([exn:fail:object? (λ (exn) (hash-set! (get-field _hash o) i v))]) (dynamic-set-field! i o v)))
            (define (ref-keys o) (append (remove '_hash (field-names o)) (hash-keys (get-field _hash o))))]))

(module+ test
  (require rackunit racket/set)
  (define h (make-hash '((foo . 42))))
  (check-equal? (ref h 'foo) 42)
  (ref-set! h 'foo 85)
  (check-equal? (ref h 'foo) 85)
  (ref-set! h 'bar 121)
  (check-equal? (ref h 'bar) 121)
  (check-equal? (apply set (ref-keys h)) (apply set '(foo bar)))
  (define o (make-object (class object% (super-new) (field [_hash (make-hash)][foo 42]))))
  (check-equal? (ref o 'foo) 42)
  (ref-set! o 'foo 100)
  (check-equal? (ref o 'foo) 100)
  (ref-set! o 'bar 121)
  (check-equal? (ref o 'bar) 121)
  (check-equal? (apply set (ref-keys o)) (apply set '(foo bar))))

(define (ref* c . is)
  (for/fold ([c c])
            ([i (in-list is)])
    (ref c i)))

(define (ref*-set! c . is+val)
  (match-define (list is ... i val) is+val)
  (ref-set! (apply ref* c is) i val))

(require sugar/debug)
(define (ref-set*! c . kvs)
  (for ([k (in-list kvs)]
        [v (in-list (cdr kvs))]
        [i (in-naturals)]
        #:when (even? i))
    (ref-set! c k v)))

(module+ test
  (define h2 (make-hash (list (cons 'foo (make-hash (list (cons 'bar (make-hash '((zam . 42))))))))))
  (check-equal? (ref* h2 'foo 'bar 'zam) 42)
  (ref*-set! h2 'foo 'bar 'zam 89)
  (check-equal? (ref* h2 'foo 'bar 'zam) 89)
  (ref-set*! h2 'hi 1 'there 2)
  (check-equal? (ref h2 'hi) 1)
  (check-equal? (ref h2 'there) 2))

(define-generics countable
  (length countable)
  (countable->list countable)
  #:defaults
  ([list? (define length b:length)
          (define countable->list (λ (x) x))]
   [vector? (define length vector-length)
            (define countable->list vector->list)]
   [string? (define length string-length)
            (define countable->list string->list)]
   [bytes? (define length bytes-length)
           (define countable->list bytes->list)]
   [dict? (define length dict-count)
          (define countable->list (λ (x) x))]
   [object? (define (length o) (b:length (get-field _list o)))
            (define (countable->list o) (get-field _list o))]))

(module+ test
  (require racket/list)
  (check-equal? (length (make-list 42 #f)) 42)
  (check-equal? (length (make-vector 42 #f)) 42)
  (check-equal? (length (make-string 42 #\x)) 42)
  (check-equal? (length (make-bytes 42 0)) 42)
  (check-equal? (length (map cons (range 42) (range 42))) 42)
  (check-equal? (length (make-object (class object% (super-new) (field [_list (make-list 42 #f)])))) 42))

(define-generics pushable
  (push-end pushable xs)
  #:defaults
  ([list? (define push-end b:append)]
   [object? (define (push-end o xs)
              (append (get-field _list o) xs))]))

(module+ test
  (check-equal? (push-end (range 3) '(3 4 5)) (range 6))
  (define o2 (make-object (class object% (super-new) (field [_list (range 3)]))))
  (ref-set! o2 '_list (push-end o2 '(3 4 5)))
  (check-equal? (ref o2 '_list) (range 6)))

