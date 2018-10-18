#lang debug racket
(require racket/generator sugar graph rackunit math)
(provide (all-defined-out))

(struct $csp ([vars #:mutable]
              [constraints #:mutable]) #:transparent)
(struct $constraint (names proc) #:transparent)
(struct $var (name vals) #:transparent)
(define $var-name? symbol?)
(struct $avar $var () #:transparent)
(struct inconsistency-signal (csp) #:transparent)

(define/contract (check-name-in-csp! caller csp name)
  (symbol? $csp? $var-name? . -> . void?)
  (define names (map $var-name ($csp-vars csp)))
  (unless (memq name names)
    (raise-argument-error caller (format "one of these existing csp var names: ~v" names) name)))

(define/contract ($csp-var csp name)
  ($csp? $var-name? . -> . $var?)
  (check-name-in-csp! '$csp-var csp name)
  (for/first ([var (in-list ($csp-vars csp))]
              #:when (eq? name ($var-name var)))
    var))

(define/contract ($csp-vals csp name)
  ($csp? $var-name? . -> . (listof any/c))
  (check-name-in-csp! '$csp-vals csp name)
  ($var-vals ($csp-var csp name)))

(define order-domain-values values)
(define (assign-val csp name val)
  ($csp
   (for/list ([var ($csp-vars csp)])
     (if (eq? name ($var-name var))
         ($avar name (list val))
         var))
   ($csp-constraints csp)))

(define current-select-variable (make-parameter #f))
(define current-order-values (make-parameter #f))
(define current-inference (make-parameter #f))

(define (first-unassigned-variable csp)
  (for/first ([var ($csp-vars csp)]
              #:unless ($avar? var))
    var))

(check-equal? (first-unassigned-variable ($csp (list ($var 'a (range 3)) ($var 'b (range 3))) null)) ($var 'a (range 3)))
(check-equal? (first-unassigned-variable ($csp (list ($avar 'a (range 3)) ($var 'b (range 3))) null)) ($var 'b (range 3)))
(check-false (first-unassigned-variable ($csp (list ($avar 'a (range 3)) ($avar 'b (range 3))) null)))

(define first-domain-value values)

(define (no-inference csp name) csp)

(define (forward-check csp aname)
  (define aval (first ($csp-vals csp aname)))
  (define (filter-vals var)
    (match-define ($var name vals) var)
    (define new-vals
      (match (for*/list ([constraint (in-list ($csp-constraints csp))]
                         [cnames (in-value ($constraint-names constraint))]
                         #:when (and (memq aname cnames) (memq name cnames)))
               constraint)
        [(list) vals]
        [constraints
         (for*/list ([val (in-list vals)]
                     [constraint (in-list constraints)]
                     [cnames (in-value ($constraint-names constraint))]
                     #:when (cond
                              [(eq? (first cnames) name)
                               (($constraint-proc constraint) val aval)]
                              [(eq? (second cnames) name)
                               (($constraint-proc constraint) aval val)]
                              [else #true]))
           val)]))
    (unless (pair? new-vals) (raise (inconsistency-signal csp)))
    new-vals)

  ($csp
   (for/list ([var ($csp-vars csp)])
     (if ($avar? var)
         var
         ($var ($var-name var) (filter-vals var))))
   ($csp-constraints csp)))

(check-equal?
 ($csp-vars (forward-check ($csp (list ($avar 'a '(1)) ($var 'b (range 2))) null) 'a))
 (list ($avar 'a '(1)) ($var 'b '(0 1))))

(check-equal?
 ;; no inconsistency because b≠c not checked (because fc is relative to a)
 ($csp-vars (forward-check ($csp (list ($avar 'a '(1))  ($var 'b (range 2)) ($var 'c '(0)))
                                 (list ($constraint '(a b) (negate =))
                                       ($constraint '(b c) (negate =)))) 'a))
 (list ($avar 'a '(1)) ($var 'b '(0)) ($var 'c '(0))))

(check-equal?
 ;; no inconsistency because a≠b not checked (because fc ignores a, which is already assigned)
 ($csp-vars (forward-check ($csp (list ($avar 'a '(1))  ($avar 'b '(1)) ($var 'c (range 2)))
                                 (list ($constraint '(a b) (negate =))
                                       ($constraint '(b c) (negate =)))) 'b))
 (list ($avar 'a '(1)) ($avar 'b '(1)) ($var 'c '(0))))

(check-exn inconsistency-signal?
           (λ () ($csp-vars (forward-check ($csp (list ($avar 'a '(1))
                                                       ($var 'b '(1)))
                                                 (list ($constraint '(a b) (negate =)))) 'a))))

(define/contract (backtrack-solution-generator csp
                                               [select-unassigned-variable (or (current-select-variable) first-unassigned-variable)]
                                               [order-domain-values (or (current-order-values) first-domain-value)]
                                               [inference (or (current-inference) no-inference)])
  (($csp?) (procedure? procedure? procedure?) . ->* . generator?)
  (generator ()
             (let backtrack ([csp csp])
               (match (select-unassigned-variable csp)
                 [#false (yield csp)]
                 [($var name vals)
                  (for ([val (in-list (order-domain-values vals))])
                    (with-handlers ([inconsistency-signal? void])
                      (let* ([csp (assign-val csp name val)]
                             [csp (inference csp name)])
                        (backtrack csp))))]))))

(define/contract (solve* csp [finish-proc $csp-vars][solution-limit +inf.0])
  (($csp?) (procedure? integer?) . ->* . (listof any/c))
  (for/list ([solution (in-producer (backtrack-solution-generator csp) (void))]
             [idx (in-range solution-limit)])
    (finish-proc solution)))

(define/contract (solve csp [finish-proc $csp-vars])
  (($csp?) (procedure?) . ->* . (or/c #false any/c))
  (match (solve* csp finish-proc 1)
    [(list solution) solution]
    [else #false]))

(define (<> a b) (not (= a b)))
(define (neq? a b) (not (eq? a b)))

(parameterize ([current-inference forward-check])
  (time (solve* ($csp (list ($var 'a (range 3))
                            ($var 'b (range 3))
                            ($var 'c (range 3)))
                      (list ($constraint '(a b) <>)
                            ($constraint '(a c) <>)
                            ($constraint '(b c) <>))))))
(parameterize ([current-inference forward-check])
  (define vds (for/list ([k '(wa nsw t q nt v sa)])
                ($var k '(red green blue))))
  (define cs (list
              ($constraint '(wa nt) neq?)
              ($constraint '(wa sa) neq?)
              ($constraint '(nt sa) neq?)
              ($constraint '(nt q) neq?)
              ($constraint '(q sa) neq?)
              ($constraint '(q nsw) neq?)
              ($constraint '(nsw sa) neq?)
              ($constraint '(nsw v) neq?)
              ($constraint '(v sa) neq?)))
  (define csp ($csp vds cs))
  (check-equal? (time (length (solve* csp))) 18))