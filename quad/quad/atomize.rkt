#lang debug racket/base
(require racket/contract racket/match racket/list txexpr racket/dict sugar/list racket/function
         "quad.rkt" "qexpr.rkt" "param.rkt" "generic.rkt")
(provide (all-defined-out))
(module+ test (require rackunit))

(define (update-with base-hash . update-hashes)
  ;; starting with base-hash, add or update keys found in update-hashes
  (for/hasheq ([(k v) (in-dict (append-map hash->list (list* base-hash update-hashes)))])
              (values k v)))

(module+ test
  (check-equal?
   ((hasheq 'foo "bar" 'zim "zam") . update-with .  (hasheq 'zim "BANG") (hasheq 'toe "jam") (hasheq 'foo "zay"))
   '#hasheq((zim . "BANG") (foo . "zay") (toe . "jam"))))

(define (merge-whitespace aqs [white-aq? (λ (aq) (char-whitespace? (car (elems aq))))])
  ;; collapse each sequence of whitespace aqs to the first one, and make it a space
  ;; also drop leading & trailing whitespaces
  ;; (same behavior as web browsers)
  (let loop ([acc null][aqs aqs])
    (if (null? aqs)
        (flatten acc)
        (let*-values ([(bs rest) (splitf-at aqs (negate white-aq?))]
                      [(ws rest) (splitf-at rest white-aq?)])
          (loop (list acc bs (if (and (pair? rest) ;; we precede bs (only #t if rest starts with bs, because we took the ws)
                                      (pair? bs) ;; we follow bs
                                      (pair? ws)) ;; we have ws
                                 (quad (attrs (car ws)) #\space)
                                 null)) rest)))))

(module+ test
  (check-equal? (merge-whitespace (list (q #\space) (q #\newline) (q #\H) (q #\space) (q #\newline) (q #\space) (q #\i) (q #\newline)))
                (list (q #\H) (q #\space) (q #\i))))

(define/contract (atomize qx)
  ;; normalize a quad by reducing it to one-character quads.
  ;; propagate attrs downward.
  ((or/c quad? string?) . -> . (listof atomic-quad?))
  (define atomic-quads
    (let loop ([x (if (string? qx) (q qx) qx)][attrs (current-default-attrs)])
      (match x
        [(? char? c) (list (q attrs c))]
        [(? string?) (append* (for/list ([c (in-string x)]) ;; strings are exploded
                                        (loop c attrs)))]
        [($quad this-attrs elems) ;; qexprs with attributes are recursed
         (define merged-attrs (attrs . update-with . this-attrs))
         (append* (for/list ([elem (in-list elems)])
                            (loop elem merged-attrs)))]
        [else (raise-argument-error 'atomize "valid item" x)])))
  (merge-whitespace atomic-quads))

(module+ test
  (require rackunit)
  (check-equal? (atomize (q "Hi")) (list (q #\H) (q #\i)))
  (check-equal? (atomize (q "Hi " (q "You"))) (list (q #\H) (q #\i) (q #\space) (q #\Y) (q #\o) (q #\u)))
  (check-exn exn:fail:contract? (λ () (atomize #t)))
  (check-equal? (atomize (q "H i")) (list (q #\H) (q #\space) (q #\i)))
  (check-equal? (atomize (q "H \n\n i")) (list (q #\H) (q #\space) (q #\i))) ;; collapse whitespace to single

  ;; with attributes
  (check-equal? (atomize (q (hasheq 'k "v") "Hi")) (list (q (hasheq 'k "v") #\H) (q (hasheq 'k "v") #\i)))
  (check-equal? (atomize (q (hasheq 'k "v") "Hi " (q "You")))
                (list
                 ($quad '#hasheq((k . "v")) '(#\H))
                 ($quad '#hasheq((k . "v")) '(#\i))
                 ($quad '#hasheq((k . "v")) '(#\space))
                 ($quad '#hasheq((k . "v")) '(#\Y))
                 ($quad '#hasheq((k . "v")) '(#\o))
                 ($quad '#hasheq((k . "v")) '(#\u))))
  (check-equal? (atomize (q (hasheq 'k1 "v1" 'k2 42) "Hi \n\n" (q (hasheq 'k1 "v2" 'k3 "foo") "\n \nYou")))
                (list
                 ($quad '#hasheq((k1 . "v1") (k2 . 42)) '(#\H))
                 ($quad '#hasheq((k1 . "v1") (k2 . 42)) '(#\i))
                 ($quad '#hasheq((k1 . "v1") (k2 . 42)) '(#\space))
                 ($quad '#hasheq((k1 . "v2") (k2 . 42) (k3 . "foo")) '(#\Y))
                 ($quad '#hasheq((k1 . "v2") (k2 . 42) (k3 . "foo")) '(#\o))
                 ($quad '#hasheq((k1 . "v2") (k2 . 42) (k3 . "foo")) '(#\u)))))

(define whitespace-pat #px"\\s+")
(define (merge-white str) (regexp-replace* whitespace-pat str " "))

(define (isolate-white str)
  (for/list ([m (in-list (regexp-match* " " str #:gap-select? #t))]
             #:when (positive? (string-length m)))
            m))

(define (merge-adjacent-strings xs [isolate-white? #false])
  (let loop ([xs xs][acc null])
    (match xs
      [(== empty) (reverse acc)]
      [(list (? string? strs) ..1 others ...)
       (loop others (append (reverse ((if isolate-white?
                                           (compose1 isolate-white merge-white)
                                           list) (apply string-append strs))) acc))]
      [(cons x others) (loop others (cons x acc))])))
  
(define (runify qx)
  ;; runify a quad by reducing it to a series of "runs",
  ;; which are multi-character quads with the same formatting.
  (dropf
   (let loop ([x (if (string? qx) (q qx) qx)][attrs (current-default-attrs)])
     (match x
       [($quad this-attrs elems) ;; qexprs with attributes are recursed
        (define merged-attrs (attrs . update-with . this-attrs))
        (append* (for/list ([elem (in-list (merge-adjacent-strings elems 'merge-white))])
                           (if (string? elem)
                               (list (q merged-attrs elem))
                               (loop elem merged-attrs))))]))
   (λ (q) (string=? " " (car (elems q))))))

(module+ test
  (check-equal?
   (runify  (q (hasheq 'foo 42) (q "Hi" "    idiot" (q (hasheq 'bar 84) "There") "Eve" "ry" "one")))
   (list (q (hasheq 'foo 42) "Hi") (q (hasheq 'foo 42) " ") (q (hasheq 'foo 42) "idiot") (q (hasheq 'foo 42 'bar 84) "There") (q (hasheq 'foo 42) "Everyone"))))
   
