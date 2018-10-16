#lang debug racket
(require racket/generator sugar graph)
(provide (all-defined-out))

(struct $csp (variables domains neighbors constraints initial curr_domains nassigns nchecks current graph) #:transparent #:mutable)
;; `current` = current assignment
(define assignment? hash?)
(define variable? symbol?)
(define removal? (cons/c variable? any/c))

(struct $constraint (names proc) #:transparent)
(struct $vd (name vals) #:transparent)

(define (constraint-graph variables constraints)
  (for*/fold ([g (unweighted-graph/undirected variables)])
             ([constraint (in-list constraints)]
              [edge (in-combinations ($constraint-names constraint) 2)])
    (apply add-edge! g edge)
    g))

(define/contract (make-csp vds constraints)
  ((listof $vd?) (listof $constraint?) . -> . $csp?)
  (define variables (map $vd-name vds))
  (define domains (for/hasheq ([vd (in-list vds)])
                    (match vd
                      [($vd name vals) (values name vals)])))
  (define g (constraint-graph variables constraints))
  (define neighbors (for/hasheq ([v (in-list variables)])
                      (values v (get-neighbors g v))))
  ($csp variables domains neighbors constraints null #f 0 0 #f g))

(define/contract (domain csp var)
  ($csp? variable? . -> . (listof any/c))
  (hash-ref ($csp-domains csp) var))

(define/contract (curr_domain csp var)
  ($csp? variable? . -> . (listof any/c))
  (hash-ref ($csp-curr_domains csp) var))

(define/contract (neighbors csp var)
  ($csp? variable? . -> . (listof variable?))
  (hash-ref ($csp-neighbors csp) var))

(define/contract (assigns? assignment var)
  (assignment? variable? . -> . boolean?)
  (hash-has-key? assignment var))

(define nassigns $csp-nassigns)
(define nchecks $csp-nchecks)

(define/contract (check-constraint csp . varvals)
  (($csp?) #:rest (listof any/c) . ->* . any/c)
  (define varval-hash (apply hasheq varvals))
  (define relevant-constraints
    (for/list ([constraint (in-list ($csp-constraints csp))]
               #:when (for/and ([cname (in-list ($constraint-names constraint))])
                        (memq cname (hash-keys varval-hash))))
      constraint))
  (begin0
    (for/and ([constraint (in-list relevant-constraints)])
      (define vals (for/list ([cname (in-list ($constraint-names constraint))])
                     (hash-ref varval-hash cname)))
      (apply ($constraint-proc constraint) vals))
    (set-$csp-nchecks! csp (add1 ($csp-nchecks csp)))))

(define/contract (reset-counters! csp)
  ($csp? . -> . void?)
  (set-$csp-nassigns! csp 0)
  (set-$csp-nchecks! csp 0))

(define/contract (assign csp var val assignment)
  ($csp? variable? any/c assignment? . -> . void?)
  ;; Add {var: val} to assignment; Discard the old value if any.
  (hash-set! assignment var val)
  (set-$csp-nassigns! csp (add1 ($csp-nassigns csp))))

(define/contract (unassign csp var assignment)
  ($csp? variable? assignment? . -> . void?)
  ;; Remove {var: val} from assignment.
  ;; DO NOT call this if you are changing a variable to a new value;
  ;; just call assign for that.
  (hash-remove! assignment var))

(define/contract (all-variables-assigned? csp assignment)
  ($csp? assignment? . -> . boolean?)
  (= (length (hash-keys assignment)) (length ($csp-variables csp))))


(define/contract (nconflicts csp var val assignment)
  ($csp? variable? any/c assignment? . -> . number?)
  ;; Return the number of conflicts var=val has with other variables."""
  ;; Subclasses may implement this more efficiently
  (for/sum ([v (in-list (neighbors csp var))]
            #:when (assignment . assigns? . v))
    #;(define this (apply check-constraint csp (append (list var val) (flatten (for/list ([(k v) (in-hash assignment)]
                                                                               #:unless (eq? var k))
                                                                      (list k v))))))
    (define that (check-constraint csp var val v (hash-ref assignment v)))
    (if that
        0
        1)))


(define (display csp assignment)
  (displayln csp))

;; These methods are for the tree and graph-search interface:

(struct $action (var val) #:transparent #:mutable)
(define/contract (state->assignment state)
  ((listof $action?) . -> . assignment?)
  (for/hasheq ([action (in-list state)])
    (match action
      [($action var val) (values var val)])))

;; todo: test that this works
(define/contract (actions csp state)
  ($csp? (listof $action?) . -> . any/c)
  ;; Return a list of applicable actions: nonconflicting
  ;; assignments to an unassigned variable.
  (cond
    [(all-variables-assigned? csp state) empty]
    [else
     (define assignment (state->assignment state))
     (define var (for/first ([var (in-list ($csp-variables csp))]
                             #:unless (assignment . assigns? . var))
                   var))
     (for/list ([val (in-list (domain csp var))]
                #:when (zero? (nconflicts csp var val assignment)))
       ($action var val))]))

;; todo: test that this works
(define/contract (result csp state action)
  ($csp? (listof $action?) $action? . -> . assignment?)
  ;; Perform an action and return the new state.
  (match-define ($action var val) action)
  (append state (list action)))

;; todo: test that this works
(define/contract (goal_test csp state)
  ($csp? (or/c assignment? (listof $action?)) . -> . boolean?)
  ;; The goal is to assign all variables, with all constraints satisfied.
  (define assignment (if (assignment? state) state (state->assignment state)))
  (and (all-variables-assigned? csp assignment)
       (for/and ([variable ($csp-variables csp)])
         (zero? (nconflicts csp variable (hash-ref assignment variable) assignment)))))

;; These are for constraint propagation

(define/contract (support_pruning csp)
  ($csp? . -> . void?)
  ;; Make sure we can prune values from domains. (We want to pay
  ;; for this only if we use it.)
  (unless ($csp-curr_domains csp)
    (define h (make-hasheq))
    (for ([v ($csp-variables csp)])
      (hash-set! h v (hash-ref ($csp-domains csp) v)))
    (set-$csp-curr_domains! csp h)))
      
(define/contract (suppose csp var value)
  ($csp? variable? any/c . -> . (box/c (listof removal?)))
  ;; Start accumulating inferences from assuming var=value
  (support_pruning csp)
  (begin0
    (box (for/list ([val (in-list (curr_domain csp var))]
                    #:when (not (equal? val value)))
           (cons var val)))
    (hash-set! ($csp-curr_domains csp) var (list value))))

(define/contract (prune csp var value removals)
  ($csp? variable? any/c (or/c #f (box/c (listof removal?))) . -> . (box/c (listof removal?)))
  ;; Rule out var=value
  (hash-update! ($csp-curr_domains csp) var (λ (vals) (remove value vals)))
  (when removals
    (set-box! removals (append (unbox removals) (list (cons var value)))))
  removals)

(define/contract (choices csp var)
  ($csp? variable? . -> . (listof any/c))
  ;; Return all values for var that aren't currently ruled out.
  (hash-ref (or ($csp-curr_domains csp) ($csp-domains csp)) var))

(define/contract (infer_assignment csp)
  ($csp? . -> . assignment?)
  ;; Return the partial assignment implied by the current inferences.
  (support_pruning csp)
  (define assignment (make-hasheq))
  (for ([v (in-list ($csp-variables csp))])
    (match (curr_domain csp v)
      [(list one-value) (hash-set! assignment v one-value)]
      [else #f]))
  assignment)

(define/contract (restore csp removals)
  ($csp? (box/c (listof removal?)) . -> . void?)
  ;; Undo a supposition and all inferences from it.
  (for ([removal (in-list (unbox removals))])
    (match removal
      [(cons B b) (hash-update! ($csp-curr_domains csp) B
                                (λ (vals) (append vals (list b))))])))

;; This is for min_conflicts search
(define/contract (conflicted_vars csp current)
  ($csp? hash? . -> . (listof variable?))
  ;; Return a list of variables in current assignment that are in conflict
  (for/list ([var (in-list ($csp-variables csp))]
             #:when (positive? (nconflicts csp var (hash-ref current var) current)))
    var))

;; ______________________________________________________________________________
;; Constraint Propagation with AC-3

(struct $arc (start end) #:transparent)
(define/contract (AC3 csp [queue #f][removals #f])
  (($csp?) ((or/c #f (listof any/c)) (box/c (listof removal?))) . ->* . boolean?)
  (support_pruning csp)
  (with-handlers ([boolean? values])
    (for/fold ([queue (or queue
                          (for*/list ([Xi (in-list ($csp-variables csp))]
                                      [Xk (in-list (neighbors csp Xi))])
                            ($arc Xi Xk)))]
               #:result #true)
              ([i (in-naturals)]
               #:break (empty? queue))
      (match-define (cons ($arc Xi Xj) other-arcs) queue)
      (cond
        [(revise csp Xi Xj removals)
         (when (empty? (curr_domain csp Xi))
           (raise #false))
         (append other-arcs
                 (for/list ([Xk (in-list (neighbors csp Xi))]
                            #:unless (eq? Xk Xj))
                   ($arc Xk Xi)))]
        [else other-arcs]))))

(define/contract (revise csp Xi Xj removals)
  ($csp? variable? variable? (box/c (listof removal?)) . -> . boolean?)
  ;; Return true if we remove a value.
  (for/fold ([revised #false])
            ([x (in-list (curr_domain csp Xi))])
    ;; If Xi=x is consistent with Xj=y for any y, keep Xi=x, otherwise prune
    (cond
      [(not
        (for/or ([y (in-list (curr_domain csp Xj))])
          (check-constraint csp Xi x Xj y)))
       (prune csp Xi x removals)
       #true]
      [else revised])))
;; ______________________________________________________________________________
;; CSP Backtracking Search

;; Variable ordering

(define/contract (first_unassigned_variable assignment csp)
  (assignment? $csp? . -> . (or/c #false variable?))
  ;; The default variable order.
  (for/first ([var (in-list ($csp-variables csp))]
              #:unless (assignment . assigns? . var))
    var))

(define current-shuffle (make-parameter #t))

(define/contract (argmin_random_tie proc xs)
  (procedure? (listof any/c) . -> . any/c)
  (define ordered-xs (sort xs < #:key proc))
  (first ((if (current-shuffle) shuffle values)
          (takef ordered-xs (λ (x) (= (proc (car ordered-xs)) (proc x)))))))

(define/contract (mrv assignment csp)
  (assignment? $csp? . -> . any/c)
  ;; Minimum-remaining-values heuristic.
  ;; with random tiebreaker.
  (define (num_legal_values var)
    (if ($csp-curr_domains csp)
        (length (curr_domain csp var))
        (for/sum ([val (in-list (domain csp var))]
                  #:when (zero? (nconflicts csp var val assignment)))
          1)))
  (struct $mrv-rec (var num) #:transparent)
  (argmin_random_tie
   (λ (var) (num_legal_values var))
   (for/list ([var (in-list ($csp-variables csp))]
              #:unless (assignment . assigns? . var))
     var)))

;; Value ordering

(define/contract (unordered_domain_values var assignment csp)
  (variable? assignment? $csp? . -> . (listof any/c))
  ;; The default value order.
  (choices csp var))

(define/contract (lcv var assignment csp)
  (variable? assignment? $csp? . -> . (listof any/c))
  ;; Least-constraining-values heuristic.
  (sort (choices csp var) < #:key (λ (val) (nconflicts csp var val assignment))))

;; Inference

(define/contract (no_inference csp var value assignment removals)
  ($csp? variable? any/c assignment? (box/c (listof removal?)) . -> . boolean?)
  #true)

(define/contract (forward_checking csp var value assignment removals)
  ($csp? variable? any/c assignment? (box/c (listof removal?)) . -> . boolean?)
  ;; Prune neighbor values inconsistent with var=value.
  (support_pruning csp) ;; necessary to set up curr_domains
  (for/and ([B (in-list (neighbors csp var))]
            #:unless (assignment . assigns? . B))
    (for ([b (in-list (curr_domain csp B))]
          #:unless (check-constraint csp var value B b))
      (prune csp B b removals))
    (not (empty? (curr_domain csp B)))))

(define/contract (mac csp var value assignment removals)
  ($csp? variable? any/c assignment? (box/c (listof removal?)) . -> . boolean?)
  ;; Maintain arc consistency.
  (AC3 csp (for/list ([neighbor (in-list (neighbors csp var))])
             ($arc neighbor var)) removals))
  
(define current-select-variable (make-parameter #f))
(define current-order-values (make-parameter #f))
(define current-inference (make-parameter #f))

(define/contract (backtracking_search
                  csp
                  [select_unassigned_variable (or (current-select-variable) first_unassigned_variable)]
                  [order_domain_values (or (current-order-values) unordered_domain_values)]
                  [inference (or (current-inference) no_inference)])
  (($csp?) (procedure? procedure? procedure?) . ->* . generator?)
  (generator ()
             (let backtrack ([assignment (make-hasheq)])
               (cond
                 [(all-variables-assigned? csp assignment)
                  (unless (goal_test csp assignment) (error 'whut))
                  (yield (hash-copy assignment))]
                 [else
                  (define var (select_unassigned_variable assignment csp))
                  (for ([val (in-list (order_domain_values var assignment csp))]
                        #:when (zero? (nconflicts csp var val assignment)))
                    (assign csp var val assignment)
                    (define removals (suppose csp var val))
                    (when (inference csp var val assignment removals)
                      (backtrack assignment))
                    (restore csp removals))
                  (unassign csp var assignment)]))))
;; ______________________________________________________________________________
;; Min-conflicts hillclimbing search for CSPs

(define (min_conflicts csp [max_steps (expt 10 5)])
  (($csp?) (integer?) . ->* . generator?)
  ;; Solve a CSP by stochastic hillclimbing on the number of conflicts.
  ;; Generate a complete assignment for all variables (probably with conflicts)
  (generator ()
             (define current (make-hasheq))
             (set-$csp-current! csp current)
             (for ([var (in-list ($csp-variables csp))])
               (define val (min_conflicts_value csp var current))
               (assign csp var val current))
             ;; Now repeatedly choose a random conflicted variable and change it
             (for ([i (in-range max_steps)])
               (define conflicted (conflicted_vars csp current))
               (when (empty? conflicted)
                 (yield current))
               (define var (first ((if (current-shuffle) shuffle values) conflicted)))
               (define val (min_conflicts_value csp var current))
               (assign csp var val current))))
  
(define/contract (min_conflicts_value csp var current)
  ($csp? variable? hash? . -> . any/c)
  ;; Return the value that will give var the least number of conflicts.
  ;; If there is a tie, choose at random.
  (argmin_random_tie (λ (val) (nconflicts csp var val current)) (domain csp var)))

(define current-reset (make-parameter #t))
(define current-solver (make-parameter #f))

(define/contract (solve* csp [solution-limit +inf.0])
  (($csp?) (integer?) . ->* . (or/c #f (non-empty-listof any/c)))
  (define solver (or (current-solver) backtracking_search))
  (begin0
    (match (for/list ([solution (in-producer (solver csp) (void))]
                      [idx (in-range solution-limit)])
             solution)
      [(list solutions ...) solutions]
      [else #false])
    (when (current-reset)
      (set-$csp-curr_domains! csp #f))))

(define/contract (solve csp)
  ($csp? . -> . any/c)
  (match (solve* csp 1)
    [(list solution) solution]
    [else #false]))

(require rackunit)

(begin
    (define vs '(wa nsw t q nt v sa))
    (define vds (for/list ([k vs])
                  ($vd k '(red green blue))))
    (define (neq? a b) (not (eq? a b)))
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
    (define csp (make-csp vds cs))
    (check-true ($csp? csp))
    (define a (make-hasheq))
    (assign csp 'key 42 a)
    (check-equal? (hash-ref a 'key) 42)
    (unassign csp 'key a)
    (check-exn exn:fail? (λ () (hash-ref a 'key)))
    (check-equal? 0 (nconflicts csp 'wa 'red (hasheq 'wa 42)))
    (support_pruning csp)
    (check-true (hash? ($csp-curr_domains csp)))

    (check-equal? (suppose csp 'wa 'red) '#&((wa . green) (wa . blue)))
    (check-equal? (curr_domain csp 'wa) '(red))

    (check-equal? (prune csp 'v 'red (box empty)) '#&((v . red)))

    (check-equal? (choices csp 'v) '(green blue))
    (check-equal? (choices csp 'wa) '(red))
    (check-equal? (infer_assignment csp)
                  (make-hasheq '((wa . red))))
    (check-equal? (suppose csp 'v 'blue) '#&((v . green)))
    (check-equal? (infer_assignment csp)
                  (make-hasheq '((v . blue) (wa . red))))
    (restore csp '#&((wa . green)))
    (check-equal? (infer_assignment csp)
                  (make-hasheq '((v . blue))))
    (restore csp '#&((v . blue)))
    (check-equal? (infer_assignment csp) (make-hasheq))

    (check-equal? (first_unassigned_variable (hash) csp) 'wa)
    (check-equal? (unordered_domain_values 'wa (hash) csp) '(red green))

    (set-$csp-curr_domains! csp #f) ; reset current domains
    (check-equal? (solve csp)
                  (make-hasheq '((nsw . green) (nt . green) (q . red) (sa . blue) (t . blue) (v . red) (wa . red))))
    (check-equal? (begin0 (list ($csp-nassigns csp) ($csp-nchecks csp)) (reset-counters! csp)) '(40 321))
    (check-equal? (length (solve* csp)) 18)

    (check-equal? (suppose csp 'nsw 'red) '#&((nsw . green) (nsw . blue)))
    (check-equal? (solve csp)
                  (make-hasheq '((nsw . red) (nt . red) (q . green) (sa . blue) (t . blue) (v . green) (wa . green))))
    (check-equal? ($csp-nassigns csp) 368)

    (reset-counters! csp)
    (check-equal? (suppose csp 'nsw 'red) '#&((nsw . green) (nsw . blue)))
    (check-equal? (length (solve* csp)) 6)
    (check-equal? (begin0 (list ($csp-nassigns csp) ($csp-nchecks csp)) (reset-counters! csp)) '(111 1035))

    (parameterize ([current-select-variable mrv]
                   [current-shuffle #f])
      (check-equal?
       (solve csp)
       (make-hasheq '((nsw . green) (nt . green) (q . red) (sa . blue) (t . blue) (v . red) (wa . red))))
      (check-equal? (begin0 (list ($csp-nassigns csp) ($csp-nchecks csp)) (reset-counters! csp)) '(39 321)))

    (parameterize ([current-order-values lcv])
      (check-equal?
       (solve csp)
       (make-hasheq '((nsw . green) (nt . green) (q . red) (sa . blue) (t . blue) (v . red) (wa . red))))
      (check-equal? (begin0 (list ($csp-nassigns csp) ($csp-nchecks csp)) (reset-counters! csp)) '(39 1040)))

    (parameterize ([current-inference forward_checking])
      (forward_checking csp 'sa 'blue (make-hasheq) (box null))
      (check-equal? ($csp-curr_domains csp)
                    (make-hasheq '((nsw . (red green)) (nt . (red green)) (q . (red green)) (sa . (red green blue)) (t . (red green blue)) (v . (red green)) (wa . (red green))))))

    (set-$csp-curr_domains! csp #f)
    (parameterize ([current-inference forward_checking])
      (check-equal?
       (solve csp)
       (make-hasheq '((nsw . green) (nt . green) (q . red) (sa . blue) (t . blue) (v . red) (wa . red))))
      (check-equal? (begin0 (list ($csp-nassigns csp) ($csp-nchecks csp)) (reset-counters! csp)) '(25 121)))

    (set-$csp-curr_domains! csp #f)
    (parameterize ([current-inference mac]
                   [current-reset #f])
      (check-equal? (solve csp)
                    (make-hasheq '((nsw . green) (nt . green) (q . red) (sa . blue) (t . blue) (v . red) (wa . red))))
      (check-equal? (begin0 (list ($csp-nassigns csp) ($csp-nchecks csp)) (reset-counters! csp)) '(17 175)))

    (parameterize ([current-select-variable mrv]
                   [current-order-values lcv]
                   [current-inference mac]
                   [current-reset #f])
      (check-equal? (solve csp)
                    (make-hasheq '((nsw . green) (nt . green) (q . red) (sa . blue) (t . blue) (v . red) (wa . red))))
      (check-equal? (begin0 (list ($csp-nassigns csp) ($csp-nchecks csp)) (reset-counters! csp)) '(7 45)))

    (set-$csp-curr_domains! csp #f)
    (parameterize ([current-shuffle #f]
                   [current-solver min_conflicts])
      (check-equal?
       (solve csp)
       (make-hasheq '((nsw . red) (nt . red) (q . green) (sa . blue) (t . red) (v . green) (wa . green))))
      (check-equal? (begin0 (list ($csp-nassigns csp) ($csp-nchecks csp)) (reset-counters! csp)) '(9 220))))

(solve (make-csp (list ($vd 'a '(1 2 3))
                       ($vd 'b '(4 5 6))
                       ($vd 'c '(7 8 9)))
                 (list ($constraint '(a b c) (λ (a b c) (= (+ a b c) 18))))))