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
  #:defaults
  ([hash? (define ref hash-ref)
          (define ref-set! hash-set!)]
   [object? (define (ref o i) (with-handlers ([exn:fail:object? (λ (exn) (hash-ref (get-field _hash o) i))]) (dynamic-get-field i o)))
            (define (ref-set! o i v) (with-handlers ([exn:fail:object? (λ (exn) (hash-set! (get-field _hash o) i v))]) (dynamic-set-field! i o v)))]))

(module+ test
  (require rackunit)
  (define h (make-hash '((foo . 42))))
  (check-equal? (ref h 'foo) 42)
  (ref-set! h 'foo 85)
  (check-equal? (ref h 'foo) 85)
  (ref-set! h 'bar 121)
  (check-equal? (ref h 'bar) 121)
  (define o (make-object (class object% (super-new) (field [_hash (make-hash)][foo 42]))))
  (check-equal? (ref o 'foo) 42)
  (ref-set! o 'foo 100)
  (check-equal? (ref o 'foo) 100)
  (ref-set! o 'bar 121)
  (check-equal? (ref o 'bar) 121))

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
  #:defaults
  ([list? (define length b:length)]
   [vector? (define length vector-length)]
   [string? (define length string-length)]
   [bytes? (define length bytes-length)]
   [dict? (define length dict-count)]
   [object? (define (length o) (with-handlers ([exn:fail:object? (λ (exn) 0)]) (b:length (get-field _list o))))]))

(module+ test
  (require racket/list)
  (check-equal? (length (make-list 42 #f)) 42)
  (check-equal? (length (make-vector 42 #f)) 42)
  (check-equal? (length (make-string 42 #\x)) 42)
  (check-equal? (length (make-bytes 42 0)) 42)
  (check-equal? (length (map cons (range 42) (range 42))) 42)
  (check-equal? (length (make-object (class object% (super-new) (field [_list (make-list 42 #f)])))) 42)
  (check-equal? (length (make-object (class object% (super-new)))) 0))
