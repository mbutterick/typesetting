#lang br

(define pointer-ks (make-hash))
(define results (make-hash))

(define vals '(a b (h) d e f g h i j))

(for ([(val i) (in-indexed vals)])
  (hash-ref! results i (λ ()
                    (report* results i)
                    (let/cc pointer-k
                      (let ([v (cond
                                 [(pair? val) (hash-set! pointer-ks (car val) pointer-k) (car val)]
                                 [else val])])
                        (define pointer (hash-ref pointer-ks v #f))
                        (cond
                          [pointer (hash-remove! pointer-ks v)
                                   (pointer (format "~a xref" v))]
                          [else v]))))))

(for/list ([i (in-range (length (hash-keys results)))])
  (hash-ref results i))