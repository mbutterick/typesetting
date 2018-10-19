#lang debug racket
(require racket/generator sugar graph rackunit math)
(provide (all-defined-out))

(struct $csp ([vars #:mutable]
              [constraints #:mutable]) #:transparent)
(struct $constraint (names proc) #:transparent)
(struct $var (name vals past conflicts) #:transparent)
(define (+$var name vals [past null] [conflicts null])
  ($var name vals past conflicts))
  
(define $var-name? symbol?)
(struct $avar $var () #:transparent)
(define (+$avar name vals [past null] [conflicts null])
  ($avar name vals past conflicts))
(struct inconsistency-signal (csp) #:transparent)

(define current-select-variable (make-parameter #f))
(define current-order-values (make-parameter #f))
(define current-inference (make-parameter #f))
(define current-solver (make-parameter #f))


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
(define/contract (assign-val csp name val)
  ($csp? $var-name? any/c . -> . $csp?)
  ($csp
   (for/list ([var ($csp-vars csp)])
     (if (eq? name ($var-name var))
         (+$avar name (list val))
         var))
   ($csp-constraints csp)))

(define/contract (update-conflicts csp name conflicts)
  ($csp? $var-name? (listof $var-name?) . -> . $csp?)
  ($csp
   (for/list ([var ($csp-vars csp)])
     (match var
       [($var (? (λ (x) (eq? x name))) vals past _)
        (+$avar name vals past conflicts)]
       [else var]))
   ($csp-constraints csp)))

(define/contract (first-unassigned-variable csp)
  ($csp? . -> . (or/c #false (and/c $var? (not/c $avar?))))
  (for/first ([var (in-list ($csp-vars csp))]
              #:unless ($avar? var))
    var))

(check-equal? (first-unassigned-variable ($csp (list (+$var 'a (range 3)) (+$var 'b (range 3))) null))
              (+$var 'a (range 3)))
(check-equal? (first-unassigned-variable ($csp (list (+$avar 'a (range 3)) (+$var 'b (range 3))) null))
              (+$var 'b (range 3)))
(check-false (first-unassigned-variable ($csp (list (+$avar 'a (range 3)) (+$avar 'b (range 3))) null)))

(define first-domain-value values)

(define (no-inference csp name) csp)

(define/contract (relating constraints names)
  ((listof $constraint?) (listof $var-name?) . -> . (listof $constraint?))
  (for*/list ([constraint (in-list constraints)]
              [cnames (in-value ($constraint-names constraint))]
              #:when (for/and ([name (in-list names)])
                       (memq name cnames)))
    constraint))

(define/contract (forward-check csp aname)
  ($csp? $var-name? . -> . $csp?)
  #R csp
  (define aval (first ($csp-vals csp aname)))
  (define (filter-vals var)
    (match-define ($var name vals past conflicts) var)
    (match (($csp-constraints csp) . relating . (list aname name))
      [(? empty?) var]
      [constraints
       (define new-vals
         (for/list ([val (in-list vals)]
                    #:when (for/and ([constraint (in-list constraints)])
                             (let ([proc ($constraint-proc constraint)])
                               (if (eq? name (first ($constraint-names constraint)))
                                   (proc val aval)
                                   (proc aval val)))))
           val))
       (unless (pair? new-vals)
         (raise (inconsistency-signal past)))
       (+$var name new-vals (cons aname past) conflicts)]))

  ($csp
   (for/list ([var (in-list ($csp-vars csp))])
     (if ($avar? var)
         var
         (filter-vals var)))
   ($csp-constraints csp)))

(check-equal?
 ;; no forward checking when no constraints
 ($csp-vars (forward-check ($csp (list (+$avar 'a '(1)) (+$var 'b (range 2))) null) 'a))
 (list (+$avar 'a '(1)) (+$var 'b '(0 1))))

(check-equal?
 ($csp-vars (forward-check (forward-check ($csp (list (+$avar 'a '(1))  (+$avar 'b '(0)) (+$var 'c '(0 1 2)))
                                                (list ($constraint '(a c) (negate =))
                                                      ($constraint '(b c) (negate =)))) 'a) 'b))
 (list (+$avar 'a '(1)) (+$avar 'b '(0) '()) (+$var 'c '(2) '(b a))))

(check-equal?
 ;; no inconsistency: b≠c not checked when fc is relative to a
 ($csp-vars (forward-check ($csp (list (+$avar 'a '(1))  (+$var 'b (range 2)) (+$var 'c '(0)))
                                 (list ($constraint '(a b) (negate =))
                                       ($constraint '(b c) (negate =)))) 'a))
 (list (+$avar 'a '(1)) (+$var 'b '(0) '(a)) (+$var 'c '(0))))

(check-equal?
 ;; no inconsistency: a≠b not checked when fc ignores a, which is already assigned
 ($csp-vars (forward-check ($csp (list (+$avar 'a '(1))  (+$avar 'b '(1)) (+$var 'c (range 2)))
                                 (list ($constraint '(a b) (negate =))
                                       ($constraint '(b c) (negate =)))) 'b))
 (list (+$avar 'a '(1)) (+$avar 'b '(1)) (+$var 'c '(0) '(b))))

(check-exn inconsistency-signal?
           (λ () ($csp-vars (forward-check ($csp (list (+$avar 'a '(1))
                                                       (+$var 'b '(1)))
                                                 (list ($constraint '(a b) (negate =)))) 'a))))


(check-equal? ($csp-vars (forward-check ($csp (list (+$avar 'a (range 3))
                                                    (+$var 'b (range 3)))
                                              (list ($constraint '(a b) <)
                                                    ($constraint '(a b) <)
                                                    ($constraint '(a b) <))) 'a))
              (list (+$avar 'a '(0 1 2)) (+$var 'b '(1 2) '(a))))


(define/contract (backtracking-solver
                  csp
                  #:select-variable [select-unassigned-variable (or (current-select-variable) first-unassigned-variable)]
                  #:order-values [order-domain-values (or (current-order-values) first-domain-value)]
                  #:inference [inference (or (current-inference) no-inference)])
  (($csp?) (#:select-variable procedure? #:order-values procedure? #:inference procedure?) . ->* . generator?)
  (generator ()
             (let backtrack ([csp csp])
               (match (select-unassigned-variable csp)
                 [#false (yield csp)]
                 [($var name vals _ _)
                  (for/fold ([conflicts null]
                             #:result (void))
                            ([val (in-list (order-domain-values vals))])
                    #R conflicts
                    (with-handlers ([inconsistency-signal?
                                     (λ (sig)
                                       (match sig
                                         [(inconsistency-signal new-conflicts)
                                          (append new-conflicts conflicts)]))])
                      (let* ([csp (assign-val csp name val)]
                             [csp (inference csp name)])
                        (backtrack csp))
                      conflicts))]))))

(define/contract (solve* csp
                         #:finish-proc [finish-proc $csp-vars]
                         #:solver [solver (or (current-solver) backtracking-solver)]
                         #:count [max-solutions +inf.0])
  (($csp?) (#:finish-proc procedure? #:solver generator? #:count integer?) . ->* . (listof any/c))
  (for/list ([solution (in-producer (solver csp) (void))]
             [idx (in-range max-solutions)])
    (finish-proc solution)))

(define/contract (solve csp
                        #:finish-proc [finish-proc $csp-vars]
                        #:solver [solver (or (current-solver) backtracking-solver)])
  (($csp?) (#:finish-proc procedure? #:solver generator?) . ->* . (or/c #false any/c))
  (match (solve* csp #:finish-proc finish-proc #:solver solver #:count 1)
    [(list solution) solution]
    [else #false]))

(define (<> a b) (not (= a b)))
(define (neq? a b) (not (eq? a b)))

(parameterize ([current-inference forward-check])
    (time (solve* ($csp (list (+$var 'a '(1))
                              (+$var 'b '(1))
                              (+$var 'c '(1)))
                        (list ($constraint '(a b) <>)
                              ($constraint '(a c) <>)
                              ($constraint '(b c) <>))))))

(parameterize ([current-inference forward-check])
  (define vds (for/list ([k '(wa nsw t q nt v sa)])
                (+$var k '(red green blue))))
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