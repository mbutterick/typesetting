#lang restructure/racket

(define pointer-ks (make-hash))
(define results (make-hash))

(define vals '(a b (h) d e f g h i j))

(for ([(val i) (in-indexed vals)]
      #:unless (hash-has-key? results val))
  (hash-set! results i (let/cc pointer-k
                         (cond
                           [(hash-ref pointer-ks val #f) => (λ (pk)
                                                              (hash-remove! pointer-ks val)
                                                              (pk (format "~a@~a" val i)))]
                           [(pair? val) (hash-set! pointer-ks (car val) pointer-k) 'tbd]
                           [else val]))))
(test-module
 (check-equal?
  (for/list ([i (in-range (length (hash-keys results)))])
    (hash-ref results i))
  '(a b "h@7" d e f g h i j)))