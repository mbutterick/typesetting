#lang debug racket

(struct $csp ([vars #:mutable]
              [constraints #:mutable]) #:transparent)

(struct $var (name vals) #:transparent)
(define $var-name? symbol?)
(struct $constraint (names proc) #:transparent)

(define/contract (check-name-in-csp! caller csp name)
  (symbol? $csp? $var-name? . -> . void?)
  (define names (map $var-name ($csp-vars csp)))
  (unless (memq name names)
    (raise-argument-error caller (format "csp variable name: ~v" names) name)))

(define/contract (nary-constraint? constraint num)
  ($constraint? exact-nonnegative-integer? . -> . boolean?)
  (= num (length ($constraint-names constraint))))

(define/contract (unary-constraint? constraint)
  ($constraint? . -> . boolean?)
  (nary-constraint? constraint 1))

(define/contract (binary-constraint? constraint)
  ($constraint? . -> . boolean?)
  (nary-constraint? constraint 2))

(define/contract (add-var! csp name [vals empty])
  (($csp? $var-name?) ((listof any/c)) . ->* . void?)
  (when (memq name (map $var-name ($csp-vars csp)))
    (raise-argument-error 'add-var! "var that doesn't exist" name))
  (set-$csp-vars! csp (cons ($var name vals) ($csp-vars csp))))

(define/contract (add-constraint! csp proc var-names)
  ($csp? procedure? (listof $var-name?) . -> . void?)
  (for ([name (in-list var-names)])
    (check-name-in-csp! 'add-constraint! csp name))
  (set-$csp-constraints! csp (cons ($constraint var-names proc) ($csp-constraints csp))))


(define/contract (no-solutions? csp)
  ($csp? . -> . boolean?)
  (for/or ([var (in-list ($csp-vars csp))])
    (empty? ($var-vals var))))

(define/contract (check-has-solutions! csp)
  ($csp? . -> . $csp?)
  (when (no-solutions? csp) (raise 'no-solutions))
  csp)

(struct $no-solutions () #:transparent)

(define/contract (apply-unary-constraint csp constraint)
  ($csp? unary-constraint? . -> . $csp?)
  (match-define ($constraint (list constraint-name) proc) constraint)
  (define new-csp ($csp (for/list ([var (in-list ($csp-vars csp))])
                          (match-define ($var name vals) var)
                          (if (eq? name constraint-name)
                              ($var name (cond
                                           [(promise? proc) (force proc)]
                                           [else (filter proc vals)]))
                              var))
                        ;; once the constraint is applied, it can go away
                        (remove constraint ($csp-constraints csp))))
  (when (no-solutions? new-csp) (raise ($no-solutions)))
  new-csp)


(define/contract (make-node-consistent csp)
  ($csp? . -> . $csp?)
  (for/fold ([csp csp])
            ([constraint (in-list ($csp-constraints csp))]
             #:when (unary-constraint? constraint))
    (apply-unary-constraint csp constraint)))

(define/contract ($csp-vals csp name)
  ($csp? $var-name? . -> . (listof any/c))
  (check-name-in-csp! '$csp-vals csp name)
  (for/first ([var (in-list ($csp-vars csp))]
              #:when (eq? name ($var-name var)))
    ($var-vals var)))

(struct $arc (name constraint) #:transparent)

(define/contract (revise csp arc)
  ($csp? $arc? . -> . $csp?)
  (match-define ($arc name ($constraint names constraint-proc)) arc)
  (match-define (list other-name) (remove name names))
  (define proc (if (eq? name (first names)) ; name is on left
                   constraint-proc ; so val goes on left
                   (λ (val other-val) (constraint-proc other-val val)))) ; otherwise reverse arg order
  (define (satisfies-arc? val)
    (for/or ([other-val (in-list ($csp-vals csp other-name))])
      (proc val other-val)))
  (apply-unary-constraint csp ($constraint (list name)
                                           (procedure-rename
                                            satisfies-arc?
                                            (string->symbol (format "satisfies-arc-with-~a?" other-name))))))

(define/contract (binary-constraints->arcs constraints)
  ((listof binary-constraint?) . -> . (listof $arc?))
  (for*/list ([constraint (in-list constraints)]
              [name (in-list ($constraint-names constraint))])
    ($arc name constraint)))

(define/contract (terminating-at arcs name)
  ((listof $arc?) $var-name? . -> . (listof $arc?))
  (for/list ([arc (in-list arcs)]
             #:when (eq? name (second ($constraint-names ($arc-constraint arc)))))
    arc))

(define/contract (ac-3 csp)
  ($csp? . -> . $csp?)
  ;; as described by AIMA @ 265
  (define all-arcs (binary-constraints->arcs (filter binary-constraint? ($csp-constraints csp))))
  (for/fold ([csp csp]
             [arcs all-arcs]
             #:result csp)
            ([i (in-naturals)]
             #:break (empty? arcs))
    (match-define (cons arc other-arcs) arcs)
    (match-define ($arc name _) arc)
    (define revised-csp (revise csp arc))
    (values revised-csp (if (= (length ($csp-vals csp name)) (length ($csp-vals revised-csp name)))
                            ;; revision did not reduce the domain, so keep going
                            other-arcs
                            ;; revision reduced the domain, so supplement the list of arcs
                            (remove-duplicates (append (all-arcs . terminating-at . name) other-arcs))))))

(define/contract (make-arc-consistent csp)
  ($csp? . -> . $csp?)
  ;; csp is arc-consistent if every pair of variables (x y)
  ;; has values in their domain that satisfy every binary constraint
  (ac-3 csp))

(define/contract (var-assigned? var)
  ($var? . -> . boolean?)
  (= 1 (length ($var-vals var))))

(define/contract (assignment-complete? csp)
  ($csp? . -> . boolean?)
  (andmap var-assigned? ($csp-vars csp)))

(define/contract (unassigned-vars csp)
  ($csp? . -> . (listof $var?))
  (filter-not var-assigned? ($csp-vars csp)))

(define/contract (select-unassigned-var csp)
  ($csp? . -> . $var?)
  ;; minimum remaining values (MRV) rule
  (argmin (λ (var) (length ($var-vals var))) (unassigned-vars csp)))

(define/contract (order-domain-values vals)
  ((listof any/c) . -> . (listof any/c))
  ;; todo: least constraining value sort
  vals)

;; todo: inferences between assignments
(define inference values)

(define/contract (assign-val csp name val)
  ($csp? $var-name? any/c . -> . $csp?)
  (apply-unary-constraint csp ($constraint (list name) (delay (list val)))))

(define/contract (assignment-consistent? csp name)
  ($csp? $var-name? . -> . boolean?)
  (define assigned-names (for/list ([var (in-list ($csp-vars csp))]
                                    #:when (= 1 (length ($var-vals var))))
                           ($var-name var)))
  (define constraints-to-check
    (for/list ([constraint (in-list ($csp-constraints csp))]
               #:when (match-let ([($constraint constraint-names _) constraint])
                        (and
                         (memq name constraint-names)
                         (for/and ([constraint-name (in-list constraint-names)])
                           (memq constraint-name assigned-names)))))
      constraint))
  ;; todo: remove constraints after testing and return reduced csp instead of boolean
  (for/and ([constraint (in-list constraints-to-check)])
    (match-define ($constraint names pred) constraint)
    (apply pred (for/list ([name (in-list names)])
                  (car ($csp-vals csp name))))))
                         

(define/contract (backtrack csp)
  ($csp? . -> . $csp?)
  (cond
    [(assignment-complete? csp) csp]
    [(match-let ([($var name vals) (select-unassigned-var csp)])
       (for/or ([val (in-list (order-domain-values vals))])
         (with-handlers ([$no-solutions? (λ (exn) #f)])
           (define new-csp (assign-val csp name val))
           (and (assignment-consistent? new-csp name)
                (backtrack (inference new-csp))))))]
    [else (raise ($no-solutions))]))
     

(define/contract (solve csp)
  ($csp? . -> . any/c)
  (backtrack (make-arc-consistent (make-node-consistent csp))))


(define csp ($csp empty empty))

(define digits (range 7))
(add-var! csp 't digits)
(add-var! csp 'w digits)
(add-var! csp 'o '(2 6 7))

(define (sum-three t w o) (= 3 (+ t w o)))
(add-constraint! csp sum-three '(t w o))

(define diff (compose1 not =))
(add-constraint! csp diff '(t w))
(add-constraint! csp diff '(w o))
(add-constraint! csp diff '(t o))

(add-constraint! csp < '(t w))

(define three-or-less (curryr <= 3))
(add-constraint! csp three-or-less '(t))
(add-constraint! csp three-or-less '(w))
(add-constraint! csp three-or-less '(o))

csp
(solve csp)