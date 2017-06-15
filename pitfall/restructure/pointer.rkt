#lang br

(define the-ks (make-hash))
(define xs (make-hash))

(define is '(a b (c . h) d e f g h i j))
(for ([(v i) (in-indexed is)]
      #:unless (hash-has-key? xs i))
  (report* xs i)
  (hash-set! xs i
             (let/cc k
               (let ([v (cond
                         [(pair? v) (hash-set! the-ks (cdr v) k) (car v)]
                         [else v])])
                 (define tk (hash-ref the-ks v #f))
                 (cond
                   [tk 
                    (hash-remove! the-ks v)
                    (tk (format "~a xref" v))]
                   [else (format "just ~a" v)])))))