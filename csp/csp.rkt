#lang debug racket
(require racket/generator)
(provide (all-defined-out))
(struct $csp ([vars #:mutable]
              [constraints #:mutable]) #:transparent)

(define (make-csp) ($csp null null))

(struct $var (name vals) #:transparent)
(define $var-name? symbol?)
(struct $constraint (names proc) #:transparent
  #:property prop:procedure
  (λ (constraint csp)
    (unless ($csp? csp)
      (raise-argument-error '$constraint-proc "$csp" csp))
    (match-define ($constraint names proc) constraint)
    (cond
      [(empty? names) (proc)]
      [else
       (match-define (cons name other-names) names)
       (for/and ([val (in-list ($csp-vals csp name))])
                ;; todo: reconsider efficiency of currying every value
                (($constraint other-names (curry proc val)) csp))])))

(define/contract (check-name-in-csp! caller csp name)
  (symbol? $csp? $var-name? . -> . void?)
  (define names (map $var-name ($csp-vars csp)))
  (unless (memq name names)
    (raise-argument-error caller (format "csp variable name: ~v" names) name)))

(define (nary-constraint? constraint n)
  (= n (length ($constraint-names constraint))))

(define/contract (unary-constraint? constraint)
  ($constraint? . -> . boolean?)
  (nary-constraint? constraint 1))

(define/contract (binary-constraint? constraint)
  ($constraint? . -> . boolean?)
  (nary-constraint? constraint 2))

(define/contract (add-vars! csp names [vals-or-procedure empty])
  (($csp? (listof $var-name?)) ((or/c (listof any/c) procedure?)) . ->* . void?)
  (for/fold ([vars ($csp-vars csp)]
             #:result (set-$csp-vars! csp vars))
            ([name (in-list names)])
    (when (memq name (map $var-name vars))
      (raise-argument-error 'add-vars! "var that doesn't exist" name))
    (append vars
            (let ([vals (if (procedure? vals-or-procedure)
                            (vals-or-procedure)
                            vals-or-procedure)])
              (list ($var name vals))))))

(define/contract (add-var! csp name [vals-or-procedure empty])
  (($csp? $var-name?) ((or/c (listof any/c) procedure?)) . ->* . void?)
  (add-vars! csp (list name) vals-or-procedure))

(define/contract (add-constraints! csp proc namess)
  ($csp? procedure? (listof (listof $var-name?)) . -> . void?)
  (set-$csp-constraints! csp (append ($csp-constraints csp) 
                                     (for/list ([names (in-list namess)])
                                               (for ([name (in-list names)])
                                                    (check-name-in-csp! 'add-constraint! csp name))
                                               ($constraint names proc)))))

(define/contract (add-pairwise-constraint! csp proc var-names)
  ($csp? procedure? (listof $var-name?) . -> . void?)
  (add-constraints! csp proc (combinations var-names 2)))

(define/contract (add-constraint! csp proc var-names)
  ($csp? procedure? (listof $var-name?) . -> . void?)
  (add-constraints! csp proc (list var-names)))

(define/contract (no-solutions? csp)
  ($csp? . -> . boolean?)
  (for/or ([var (in-list ($csp-vars csp))])
          (empty? ($var-vals var))))

(struct inconsistency-error () #:transparent)

(define/contract (apply-unary-constraint csp constraint)
  ($csp? unary-constraint? . -> . $csp?)
  (match-define ($constraint (list constraint-name) proc) constraint)
  (define new-csp ($csp (for/list ([var (in-list ($csp-vars csp))])
                                  (match-define ($var name vals) var)
                                  (if (eq? name constraint-name)
                                      ;; special rule: use promise for a constant value
                                      ;; to skip the filtering
                                      ($var name (if (promise? proc)
                                                     (force proc)
                                                     (filter proc vals)))
                                      var))
                        ;; once the constraint is applied, it can go away
                        ;; ps this is not the same as an "assigned" constraint
                        ;; because the var may still have multiple values
                        (remove constraint ($csp-constraints csp))))
  (when (no-solutions? new-csp) (raise (inconsistency-error)))
  new-csp)

(define/contract (make-nodes-consistent csp)
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

(define/contract (reduce-domains-by-arc csp arc)
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

(define/contract (constraint-assigned? csp constraint)
  ($csp? $constraint? . -> . any/c)
  (for/and ([name (in-list ($constraint-names constraint))])
           (memq name (map $var-name (assigned-vars csp)))))

(define/contract (remove-extraneous-constraints csp)
  ($csp? . -> . $csp?)
  ($csp
   ($csp-vars csp)
   (for/list ([constraint (in-list ($csp-constraints csp))]
              #:unless (constraint-assigned? csp constraint))
             constraint)))

(define/contract (ac-3 csp)
  ($csp? . -> . $csp?)
  ;; as described by AIMA @ 265
  (define all-arcs (binary-constraints->arcs (filter binary-constraint? ($csp-constraints csp))))
  (for/fold ([csp csp]
             [arcs all-arcs]
             #:result (remove-extraneous-constraints csp))
            ([i (in-naturals)]
             #:break (empty? arcs))
    (match-define (cons arc other-arcs) arcs)
    (match-define ($arc name _) arc)
    (define reduced-csp (reduce-domains-by-arc csp arc))
    (values reduced-csp (if (= (length ($csp-vals csp name)) (length ($csp-vals reduced-csp name)))
                            ;; revision did not reduce the domain, so keep going
                            other-arcs
                            ;; revision reduced the domain, so supplement the list of arcs
                            (remove-duplicates (append (all-arcs . terminating-at . name) other-arcs))))))

(define/contract (make-arcs-consistent csp)
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

(define (assigned-helper csp) (partition var-assigned? ($csp-vars csp)))

(define/contract (unassigned-vars csp)
  ($csp? . -> . (listof $var?))
  (match-define-values (assigned unassigned) (assigned-helper csp))
  unassigned)

(define/contract (assigned-vars csp)
  ($csp? . -> . (listof $var?))
  (match-define-values (assigned unassigned) (assigned-helper csp))
  assigned)

(define/contract (select-unassigned-var csp)
  ($csp? . -> . $var?)
  (define uvars (unassigned-vars csp))
  (when (empty? uvars)
    (raise-argument-error 'select-unassigned-var "csp with unassigned vars" csp))
  ;; minimum remaining values (MRV) rule
  (argmin (λ (var) (length ($var-vals var))) uvars))

(define/contract (order-domain-values vals)
  ((listof any/c) . -> . (listof any/c))
  ;; todo: least constraining value sort
  vals)

(define/contract (constraint-contains-name? constraint name)
  ($constraint? $var-name? . -> . boolean?)
  (and (memq name ($constraint-names constraint)) #true))

(define/contract (assign-val csp name val)
  ($csp? $var-name? any/c . -> . $csp?)
  (define csp-with-assignment (apply-unary-constraint csp ($constraint (list name) (delay (list val)))))
  (for/fold ([csp csp-with-assignment])
            ([constraint (in-list ($csp-constraints csp))]
             #:when (and (constraint-contains-name? constraint name)
                         (constraint-assigned? csp constraint)))
    (unless (constraint csp) (raise (inconsistency-error)))
    (remove-extraneous-constraints csp)))

;; todo: inferences between assignments
(define/contract (infer csp)
  ($csp? . -> . $csp?)
  (values csp))

(define/contract (backtracking-solver csp)
  ($csp? . -> . generator?)
  (generator ()
             (let backtrack ([csp (make-arcs-consistent (make-nodes-consistent csp))])
               (cond
                 [(assignment-complete? csp) (yield csp)]
                 [else ;; we have at least 1 unassigned var
                  (match-define ($var name vals) (select-unassigned-var csp))
                  (for ([val (in-list (order-domain-values vals))])
                       (with-handlers ([inconsistency-error? void])
                         (backtrack (infer (assign-val csp name val)))))]))))

(define/contract (solve* csp [finish-proc values][solution-limit +inf.0])
  (($csp?) (procedure? integer?) . ->* . (listof any/c))
  (define solutions
    (for/list ([solution (in-producer (backtracking-solver csp) (void))]
               [idx (in-range solution-limit)])
              (finish-proc solution)))
  (unless (pair? solutions) (raise (inconsistency-error)))
  solutions)

(define/contract (solve csp [finish-proc values])
  (($csp?) (procedure?) . ->* . any/c)
  (first (solve* csp finish-proc 1)))

(define ($csp-ref csp name) (car ($csp-vals csp name)))

(define/contract (alldiff x y)
  (any/c any/c . -> . boolean?)
  (not (equal? x y)))

(define/contract (alldiff= x y)
  (any/c any/c . -> . boolean?)
  (not (= x y)))

