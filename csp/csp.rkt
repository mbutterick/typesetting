#lang debug racket
(require racket/generator sugar/debug)
(provide (all-defined-out))
(struct $csp ([vars #:mutable]
              [constraints #:mutable]) #:transparent)

(define (make-csp) ($csp null null))
(define debug (make-parameter #false))

(define (in-cartesian xss)
  (in-generator (let loop ([xss xss][args empty])
                  (if (null? xss)
                      (yield (reverse args))
                      (for ([x (in-list (car xss))])
                        (loop (cdr xss) (cons x args)))))))

(struct $var (name vals) #:transparent)
(define $var-name? symbol?)
(struct $constraint (names proc) #:transparent
  #:property prop:procedure
  (λ (constraint csp)
    (unless ($csp? csp)
      (raise-argument-error '$constraint-proc "$csp" csp))
    ;; apply proc in many-to-many style
    (for/and ([args (in-cartesian (map (λ (cname) ($csp-vals csp cname)) ($constraint-names constraint)))])
      (apply ($constraint-proc constraint) args))))

(define/contract (check-name-in-csp! caller csp name)
  (symbol? $csp? $var-name? . -> . void?)
  (define names (map $var-name ($csp-vars csp)))
  (unless (memq name names)
    (raise-argument-error caller (format "one of these existing csp var names: ~v" names) name)))

(define (nary-constraint? constraint n)
  (= n (constraint-arity constraint)))

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

(define/contract (add-constraints! csp proc namess [proc-name #false])
  (($csp? procedure? (listof (listof $var-name?))) ((or/c #false $var-name?)) . ->* . void?)
  (set-$csp-constraints! csp (append ($csp-constraints csp) 
                                     (for/list ([names (in-list namess)])
                                       (for ([name (in-list names)])
                                         (check-name-in-csp! 'add-constraint! csp name))
                                       ($constraint names (if proc-name
                                                              (procedure-rename proc proc-name)
                                                              proc))))))

(define/contract (add-pairwise-constraint! csp proc var-names [proc-name #false])
  (($csp? procedure? (listof $var-name?)) ($var-name?) . ->* . void?)
  (add-constraints! csp proc (combinations var-names 2) proc-name))

(define/contract (add-constraint! csp proc var-names [proc-name #false])
  (($csp? procedure? (listof $var-name?)) ($var-name?) . ->* . void?)
  (add-constraints! csp proc (list var-names) proc-name))

(define/contract (no-solutions? csp)
  ($csp? . -> . boolean?)
  (zero? (state-count csp)))

(struct inconsistency-signal () #:transparent)

(define/contract (apply-unary-constraint csp constraint)
  ($csp? unary-constraint? . -> . $csp?)
  (match-define ($constraint (list cname) proc) constraint)
  (define new-csp ($csp (for/list ([var (in-list ($csp-vars csp))])
                          (match-define ($var name vals) var)
                          (if (eq? name cname)
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
  (when (no-solutions? new-csp) (raise (inconsistency-signal)))
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
                   constraint-proc ; so val stays on left
                   (λ (val other-val) (constraint-proc other-val val)))) ; otherwise reverse arg order
  (define (satisfies-arc? val)
    (for/or ([other-val (in-list ($csp-vals csp other-name))])
      (proc val other-val)))
  (apply-unary-constraint csp ($constraint (list name)
                                           (procedure-rename
                                            satisfies-arc?
                                            (string->symbol (format "~a-arc-to-~a" (object-name proc)  other-name))))))

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

(define/contract (remove-assigned-constraints csp [arity #false])
  (($csp?) ((or/c #false exact-nonnegative-integer?)) . ->* . $csp?)
  ($csp
   ($csp-vars csp)
   (for/list ([constraint (in-list ($csp-constraints csp))]
              #:unless (and (if arity (= arity (constraint-arity constraint)) #true)
                            (constraint-assigned? csp constraint)))
     constraint)))

(define (remove-assigned-binary-constraints csp)
  (remove-assigned-constraints csp 2))

(define/contract (ac-3 csp)
  ($csp? . -> . $csp?)
  ;; as described by AIMA @ 265
  (define all-arcs (binary-constraints->arcs (filter binary-constraint? ($csp-constraints csp))))
  (for/fold ([csp csp]
             [arcs all-arcs]
             #:result (remove-assigned-binary-constraints csp))
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
  (= 1 (remaining-values var)))

(define/contract (solution-complete? csp)
  ($csp? . -> . boolean?)
  (and (andmap var-assigned? ($csp-vars csp)) (empty? ($csp-constraints csp))))

(define (assigned-helper csp) (partition var-assigned? ($csp-vars csp)))

(define/contract (unassigned-vars csp)
  ($csp? . -> . (listof $var?))
  (match-define-values (assigned unassigned) (assigned-helper csp))
  unassigned)

(define/contract (assigned-vars csp)
  ($csp? . -> . (listof $var?))
  (match-define-values (assigned unassigned) (assigned-helper csp))
  assigned)

(define/contract (constraint-arity constraint)
  ($constraint? . -> . exact-nonnegative-integer?)
  (length ($constraint-names constraint)))

(define/contract (var-degree csp var)
  ($csp? $var? . -> . exact-nonnegative-integer?)
  (for/sum ([constraint (in-list ($csp-constraints csp))]
            #:when (constraint-contains-name? constraint ($var-name var)))
    1))

(define/contract (select-unassigned-var csp)
  ($csp? . -> . $var?)
  (define uvars (unassigned-vars csp))
  (when (empty? uvars)
    (raise-argument-error 'select-unassigned-var "csp with unassigned vars" csp))
  ;; minimum remaining values (MRV) rule
  (define uvars-by-rv (sort uvars < #:key remaining-values))
  (define minimum-remaining-values (remaining-values (first uvars-by-rv)))
  (match (takef uvars-by-rv (λ (var) (= minimum-remaining-values (remaining-values var))))
    [(list winning-uvar) winning-uvar] 
    [(list mrv-uvars ...) ;; use degree as tiebreaker
     (argmax (λ (var) (var-degree csp var)) mrv-uvars)]))

(define/contract (order-domain-values vals)
  ((listof any/c) . -> . (listof any/c))
  ;; todo: least constraining value sort
  vals)

(define/contract (constraint-contains-name? constraint name)
  ($constraint? $var-name? . -> . boolean?)
  (and (memq name ($constraint-names constraint)) #true))

(define/contract (validate-assignments csp)
  ($csp? . -> . $csp?)
  (define assigned-constraints (filter (λ (c) (constraint-assigned? csp c)) ($csp-constraints csp)))
  (for ([constraint (in-list (sort assigned-constraints < #:key constraint-arity))]
        #:unless (constraint csp))
    (raise (inconsistency-signal)))
  (reduce-constraint-arity (remove-assigned-constraints csp)))

(define/contract (assign-val csp name val)
  ($csp? $var-name? any/c . -> . $csp?)
  (define assignment-constraint ($constraint (list name) (delay (list val))))
  (validate-assignments (apply-unary-constraint csp assignment-constraint)))

(define (reduce-arity proc pattern)
  (unless (match (procedure-arity proc)
            [(arity-at-least val) (<= val (length pattern))]
            [(? number? val) (= val (length pattern))])
    (raise-argument-error 'reduce-arity (format "list of length ~a, same as procedure arity" (procedure-arity proc)) pattern))
  (define reduced-arity-name (string->symbol (format "reduced-arity-~a" (object-name proc))))
  (define-values (id-names vals) (partition symbol? pattern))
  (define new-arity (length id-names))
  (procedure-rename
   (λ xs
     (unless (= (length xs) new-arity)
       (apply raise-arity-error reduced-arity-name new-arity xs))
     (apply proc (for/fold ([acc empty]
                            [xs xs]
                            [vals vals]
                            #:result (reverse acc))
                           ([pat-item (in-list pattern)])
                   (if (symbol? pat-item)
                       (values (cons (car xs) acc) (cdr xs) vals)
                       (values (cons (car vals) acc) xs (cdr vals))))))
   reduced-arity-name))

(module+ test
  (require rackunit)
  (define f (λ (a b c d) (+ a b c d)))
  (check-equal? 10 ((reduce-arity f '(1 b c d)) 2 3 4))
  (check-equal? 10 ((reduce-arity f '(1 2 c d)) 3 4))
  (check-equal? 10 ((reduce-arity f '(1 2 3 d)) 4))
  (check-equal? 10 ((reduce-arity f '(1 b 3 d)) 2 4))
  (check-equal? 10 ((reduce-arity f '(a b 3 d)) 1 2 4)))

(define/contract (reduce-constraint-arity csp [minimum-arity 3])
  (($csp?) (exact-nonnegative-integer?) . ->* . $csp?)
  (define (assigned-name? cname) (memq cname (map $var-name (assigned-vars csp))))
  (define (partially-assigned? constraint)
    (ormap assigned-name? ($constraint-names constraint)))
  ($csp ($csp-vars csp)
        (for/list ([constraint (in-list ($csp-constraints csp))])
          (cond
            [(and (<= minimum-arity (constraint-arity constraint))
                  (partially-assigned? constraint))
             (match-define ($constraint cnames proc) constraint)
             ($constraint (filter-not assigned-name? cnames)
                          ;; pattern is mix of values and symbols (indicating variables to persist)
                          (let ([reduce-arity-pattern (for/list ([cname (in-list cnames)])
                                                        (if (assigned-name? cname)
                                                            ($csp-ref csp cname)
                                                            cname))])
                            (reduce-arity proc reduce-arity-pattern)))]
            [else constraint]))))

(module+ test
  (define creduce (assign-val ($csp (list ($var 'a '(1 2 3)) ($var 'b '(2 3)) ($var 'c '(1 2 3 4 5))) (list ($constraint '(a b c) (procedure-rename (λ (a b c) (= (+ a b c) 4)) 'summer)))) 'a 1))
  (check-equal? 
   (make-arcs-consistent (reduce-constraint-arity creduce))
   ($csp (list ($var 'a '(1)) ($var 'b '(2)) ($var 'c '(1))) '())))

;; todo: inferences between assignments
(define/contract (infer csp)
  ($csp? . -> . $csp?)
  (validate-assignments (make-arcs-consistent csp)))

(define/contract (backtracking-solver csp)
  ($csp? . -> . generator?)
  (generator ()
             (let backtrack ([csp (make-arcs-consistent (make-nodes-consistent csp))])
               (cond
                 [(solution-complete? csp) (yield csp)]
                 [else ;; we have at least 1 unassigned var
                  (match-define ($var name vals) (select-unassigned-var csp))
                  (for ([val (in-list (order-domain-values vals))])
                    (with-handlers ([inconsistency-signal? void])
                      (backtrack (infer (assign-val csp name val)))))]))))

(define/contract (solve* csp [finish-proc values][solution-limit +inf.0])
  (($csp?) (procedure? integer?) . ->* . (listof any/c))
  (define solutions
    (for/list ([solution (in-producer (backtracking-solver csp) (void))]
               [idx (in-range solution-limit)])
      (finish-proc solution)))
  (unless (pair? solutions) (raise (inconsistency-signal)))
  solutions)

(define/contract (solve csp [finish-proc values])
  (($csp?) (procedure?) . ->* . any/c)
  (first (solve* csp finish-proc 1)))

(define ($csp-ref csp name) (first ($csp-vals csp name)))

(define/contract (alldiff x y)
  (any/c any/c . -> . boolean?)
  (not (equal? x y)))

(define/contract (alldiff= x y)
  (any/c any/c . -> . boolean?)
  (not (= x y)))

(define/contract (remaining-values var)
  ($var? . -> . exact-nonnegative-integer?)
  (length ($var-vals var)))

(define/contract (state-count csp)
  ($csp? . -> . exact-nonnegative-integer?)
  (for/product ([var (in-list ($csp-vars csp))])
    (remaining-values var)))