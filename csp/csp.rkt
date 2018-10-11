#lang debug racket
(provide (all-defined-out))
(struct $csp ([vars #:mutable]
              [constraints #:mutable]) #:transparent)

(define (new-csp) ($csp null null))

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

(define/contract (nary-constraint? constraint n)
  ($constraint? exact-nonnegative-integer? . -> . boolean?)
  (= n (length ($constraint-names constraint))))

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

(define (unique-varnames? xs)
  (and (andmap $var-name? xs) (not (check-duplicates xs eq?))))

(define/contract (add-constraint! csp proc . var-names)
  (($csp? procedure?) #:rest (listof $var-name?) . ->* . void?)
  (for ([name (in-list var-names)])
    (check-name-in-csp! 'add-constraint! csp name))
  (set-$csp-constraints! csp (cons ($constraint var-names proc) ($csp-constraints csp))))

(define/contract (no-solutions? csp)
  ($csp? . -> . boolean?)
  (for/or ([var (in-list ($csp-vars csp))])
    (empty? ($var-vals var))))

(struct $csp-inconsistent () #:transparent)

(define/contract (apply-unary-constraint csp constraint)
  ($csp? unary-constraint? . -> . $csp?)
  (match-define ($constraint (list constraint-name) proc) constraint)
  (define new-csp ($csp (for/list ([var (in-list ($csp-vars csp))])
                          (match-define ($var name vals) var)
                          (if (eq? name constraint-name)
                              ($var name (if (promise? proc)
                                             (force proc)
                                             (filter proc vals)))
                              var))
                        ;; once the constraint is applied, it can go away
                        (remove constraint ($csp-constraints csp))))
  (when (no-solutions? new-csp) (raise ($csp-inconsistent)))
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
  ;; minimum remaining values (MRV) rule
  (argmin (λ (var) (length ($var-vals var))) (unassigned-vars csp)))

(define/contract (order-domain-values vals)
  ((listof any/c) . -> . (listof any/c))
  ;; todo: least constraining value sort
  vals)

;; todo: inferences between assignments
(define infer values)

(define/contract (assign-val csp name val)
  ($csp? $var-name? any/c . -> . $csp?)
  (validate-assignment (apply-unary-constraint csp ($constraint (list name) (delay (list val)))) name))

(define/contract (validate-assignment csp name)
  ($csp? $var-name? . -> . $csp?)
  (define assigned-names (map $var-name (assigned-vars csp)))
  (for/fold ([csp csp])
            ([constraint (in-list ($csp-constraints csp))]
             #:when (match-let ([($constraint cnames _) constraint])
                      (and (memq name cnames)
                           (for/and ([cname (in-list cnames)])
                             (memq cname assigned-names)))))
    (unless (constraint csp) (raise ($csp-inconsistent)))
    ($csp ($csp-vars csp) (remove constraint ($csp-constraints csp)))))
                         

(define/contract (backtrack csp)
  ($csp? . -> . $csp?)
  (cond
    [(assignment-complete? csp) csp]
    [(match-let ([($var name vals) (select-unassigned-var csp)])
       (for/or ([val (in-list (order-domain-values vals))])
         (with-handlers ([$csp-inconsistent? (λ (exn) #f)])
           (backtrack (infer (assign-val csp name val))))))]
    [else (raise ($csp-inconsistent))]))
     
(define/contract (solve csp [finish-proc values])
  (($csp?) (procedure?) . ->* . any/c)
  (finish-proc (backtrack (make-arcs-consistent (make-nodes-consistent csp)))))

(define ($csp-ref csp name)
  (car ($csp-vals csp name)))

(define/contract (alldiff . xs)
  (() #:rest (listof any/c) . ->* . boolean?)
  (for/and ([comb (in-combinations xs 2)])
   (not (apply equal? comb)))) 
