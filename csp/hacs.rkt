#lang debug racket
(require racket/generator graph sugar/debug)
(provide (all-defined-out))

(define-syntax when-debug
  (let ()
    (define debug #f)
    (if debug
        (make-rename-transformer #'begin)
        (λ (stx) (syntax-case stx ()
                   [(_ . rest) #'(void)])))))

(define-syntax-rule (in-cartesian x)
  (in-generator (let ([argss x])
                  (let loop ([argss argss][acc empty])
                    (if (null? argss)
                        (yield (reverse acc))
                        (for ([arg (in-list (car argss))])
                          (loop (cdr argss) (cons arg acc))))))))

(struct $csp (vars 
              constraints
              [assignments #:auto]
              [checks #:auto]) #:mutable #:transparent
  #:auto-value 0)
(define csp? $csp?)
(define vars $csp-vars)
(define constraints $csp-constraints)
(define-syntax-rule (in-constraints csp) (in-list ($csp-constraints csp)))
(define-syntax-rule (in-vars csp) (in-list ($csp-vars csp)))
(define-syntax-rule (in-var-names csp) (in-list (map $var-name ($csp-vars csp))))

(struct $constraint (names proc) #:transparent
  #:property prop:procedure
  (λ (constraint csp)
    (unless ($csp? csp)
      (raise-argument-error '$constraint-proc "$csp" csp))
    ;; apply proc in many-to-many style
    (for/and ([args (in-cartesian (map (λ (cname) ($csp-vals csp cname)) ($constraint-names constraint)))])
      (apply ($constraint-proc constraint) args))))

(define (make-constraint [names null] [proc values])
  ($constraint names proc))

(define constraint-names $constraint-names)
(define constraint? $constraint?)

(define (csp->graphviz csp)
  (define g (csp->graph csp))
  (graphviz g #:colors (coloring/brelaz g)))

(define (csp->graph csp)
  (for*/fold ([g (unweighted-graph/undirected (map var-name (vars csp)))])
             ([constraint (in-constraints csp)]
              [edge (in-combinations (constraint-names constraint) 2)])
    (apply add-edge! g edge)
    g))

(struct $var (name domain) #:transparent)
(define var? $var?)
(define name? symbol?)
(define $var-vals $var-domain)
(define var-name $var-name)

(struct $cvar $var (past) #:transparent)
(struct $avar $var () #:transparent)
(define assigned-var? $avar?)

(define/contract (make-csp [vars null] [constraints null])
  (() ((listof var?) (listof constraint?)) . ->* . csp?)
  ($csp vars constraints))

(define/contract (add-vars! csp names-or-procedure [vals-or-procedure empty])
  ((csp? (or/c (listof name?) procedure?)) ((or/c (listof any/c) procedure?)) . ->* . void?)
  (for/fold ([vars ($csp-vars csp)]
             #:result (set-$csp-vars! csp vars))
            ([name (in-list (if (procedure? names-or-procedure)
                                (names-or-procedure)
                                names-or-procedure))])
    (when (memq name (map var-name vars))
      (raise-argument-error 'add-vars! "var that doesn't already exist" name))
    (append vars (list ($var name
                             (if (procedure? vals-or-procedure)
                                 (vals-or-procedure)
                                 vals-or-procedure))))))

(define/contract (add-var! csp name [vals-or-procedure empty])
  ((csp? name?) ((or/c (listof any/c) procedure?)) . ->* . void?)
  (add-vars! csp (list name) vals-or-procedure))

(define/contract (add-constraints! csp proc namess [proc-name #false])
  ((csp? procedure? (listof (listof name?))) ((or/c #false name?)) . ->* . void?)
  (set-$csp-constraints! csp (append (constraints csp) 
                                     (for/list ([names (in-list namess)])
                                       (for ([name (in-list names)])
                                         (check-name-in-csp! 'add-constraints! csp name))
                                       (make-constraint names (if proc-name
                                                                  (procedure-rename proc proc-name)
                                                                  proc))))))

(define/contract (add-pairwise-constraint! csp proc var-names [proc-name #false])
  ((csp? procedure? (listof name?)) (name?) . ->* . void?)
  (add-constraints! csp proc (combinations var-names 2) proc-name))

(define/contract (add-constraint! csp proc var-names [proc-name #false])
  ((csp? procedure? (listof name?)) (name?) . ->* . void?)
  (add-constraints! csp proc (list var-names) proc-name))

(define/contract (alldiff= x y)
  (any/c any/c . -> . boolean?)
  (not (= x y)))

(struct $backtrack (names) #:transparent)
(define (backtrack! [names null]) (raise ($backtrack names)))

(define current-select-variable (make-parameter #f))
(define current-order-values (make-parameter #f))
(define current-inference (make-parameter #f))
(define current-solver (make-parameter #f))
(define current-random (make-parameter #t))
(define current-decompose (make-parameter #t))
(define current-multithreaded (make-parameter #t))

(define/contract (check-name-in-csp! caller csp name)
  (symbol? csp? name? . -> . void?)
  (define names (map var-name (vars csp)))
  (unless (memq name names)
    (raise-argument-error caller (format "one of these existing csp var names: ~v" names) name)))

(define/contract (csp-var csp name)
  (csp? name? . -> . $var?)
  (check-name-in-csp! 'csp-var csp name)
  (for/first ([var (in-vars csp)]
              #:when (eq? name (var-name var)))
    var))

(define/contract ($csp-vals csp name)
  (csp? name? . -> . (listof any/c))
  (check-name-in-csp! 'csp-vals csp name)
  ($var-domain (csp-var csp name)))

(define order-domain-values values)

(define/contract (assigned-name? csp name)
  (csp? name? . -> . any/c)
  (for/or ([var (in-vars csp)]
           #:when (assigned-var? var))
    (eq? name (var-name var))))

(define (reduce-function-arity proc pattern)
  (unless (match (procedure-arity proc)
            [(arity-at-least val) (<= val (length pattern))]
            [(? number? val) (= val (length pattern))])
    (raise-argument-error 'reduce-arity (format "list of length ~a, same as procedure arity" (procedure-arity proc)) pattern))
  (define reduced-arity-name (string->symbol (format "reduced-arity-~a" (object-name proc))))
  (define-values (boxed-id-names vals) (partition box? pattern))
  (define new-arity (length boxed-id-names))
  (procedure-rename
   (λ xs
     (unless (= (length xs) new-arity)
       (apply raise-arity-error reduced-arity-name new-arity xs))
     (apply proc (for/fold ([acc empty]
                            [xs xs]
                            [vals vals]
                            #:result (reverse acc))
                           ([pat-item (in-list pattern)])
                   (if (box? pat-item)
                       (values (cons (car xs) acc) (cdr xs) vals)
                       (values (cons (car vals) acc) xs (cdr vals))))))
   reduced-arity-name))

(define/contract (reduce-constraint-arity csp [minimum-arity 3])
  ((csp?) ((or/c #false exact-nonnegative-integer?)) . ->* . csp?)
  (let ([assigned-name? (curry assigned-name? csp)])
    (define (partially-assigned? constraint)
      (ormap assigned-name? ($constraint-names constraint)))
    (make-csp (vars csp)
              (for/list ([constraint (in-constraints csp)])
                (cond
                  [(and (or (not minimum-arity) (<= minimum-arity (constraint-arity constraint)))
                        (partially-assigned? constraint))
                   (match-define ($constraint cnames proc) constraint)
                   ($constraint (filter-not assigned-name? cnames)
                                ;; pattern is mix of values and boxed symbols (indicating variables to persist)
                                ;; use boxes here as cheap way to distinguish id symbols from value symbols
                                (let ([reduce-arity-pattern (for/list ([cname (in-list cnames)])
                                                              (if (assigned-name? cname)
                                                                  (first ($csp-vals csp cname))
                                                                  (box cname)))])
                                  (reduce-function-arity proc reduce-arity-pattern)))]
                  [else constraint])))))

(define nassns 0)
(define (reset-assns!) (set! nassns 0))
(define/contract (assign-val csp name val)
  (csp? name? any/c . -> . csp?)
  (when-debug (set! nassns (add1 nassns)))
  (make-csp
   (for/list ([var (vars csp)])
     (if (eq? name (var-name var))
         ($avar name (list val))
         var))
   (constraints csp)))

(define/contract (unassigned-vars csp)
  (csp? . -> . (listof (and/c $var? (not/c assigned-var?))))
  (filter-not assigned-var? (vars csp)))

(define/contract (first-unassigned-variable csp)
  (csp? . -> . (or/c #false (and/c $var? (not/c assigned-var?))))
  (match (unassigned-vars csp)
    [(? empty?) #false]
    [(cons x _) x]))

(define/contract (minimum-remaining-values csp)
  (csp? . -> . (or/c #false (and/c $var? (not/c assigned-var?))))
  (match (unassigned-vars csp)
    [(? empty?) #false]
    [xs (argmin (λ (var) (length ($var-domain var))) xs)]))

(define mrv minimum-remaining-values)

(define/contract (var-degree csp var)
  (csp? $var? . -> . exact-nonnegative-integer?)
  (for/sum ([constraint (in-constraints csp)]
            #:when (memq (var-name var) ($constraint-names constraint)))
    1))

(define/contract (blended-variable-selector csp)
  (csp? . -> . (or/c #false (and/c $var? (not/c assigned-var?))))
  (define uvars (unassigned-vars csp))
  (cond
    [(empty? uvars) #false]
    [(findf singleton-var? uvars)]
    [else (first (let* ([uvars-by-mrv (sort uvars < #:key (λ (var) (length ($var-domain var))))]
                        [uvars-by-degree (sort uvars-by-mrv > #:key (λ (var) (var-degree csp var)))])
                   uvars-by-degree))]))

(define/contract (remaining-values var)
  ($var? . -> . exact-nonnegative-integer?)
  (length ($var-vals var)))

(define/contract (mrv-degree-hybrid csp)
  (csp? . -> . (or/c #f $var?))
  (define uvars (unassigned-vars csp))
  (cond
    [(empty? uvars) #false]
    [else
     ;; minimum remaining values (MRV) rule
     (define mrv-arg (argmin remaining-values uvars))
     (match (filter (λ (var) (= (remaining-values mrv-arg) (remaining-values var))) uvars)
       [(list winning-uvar) winning-uvar] 
       [(list mrv-uvars ...)
        ;; use degree as tiebreaker for mrv
        (define degrees (map (λ (var) (var-degree csp var)) mrv-uvars))
        (define max-degree (apply max degrees))
        ;; use random tiebreaker for degree
        (random-pick (for/list ([var (in-list mrv-uvars)]
                                [degree (in-list degrees)]
                                #:when (= max-degree degree))
                       var))])]))
  
(define first-domain-value values)

(define (no-inference csp name) csp)

(define/contract (relating-only constraints names)
  ((listof $constraint?) (listof name?) . -> . (listof $constraint?))
  (for*/list ([constraint (in-list constraints)]
              [cnames (in-value ($constraint-names constraint))]
              #:when (and (= (length names) (length cnames))
                          (for/and ([name (in-list names)])
                            (memq name cnames))))
    constraint))

(define (binary-constraint? constraint)
  (= 2 (constraint-arity constraint)))

(define (constraint-relates? constraint name)
  (memq name ($constraint-names constraint)))

(define nfchecks 0)
(define (reset-nfcs!) (set! nfchecks 0))

(define/contract (forward-check csp aname)
  (csp? name? . -> . csp?)
  (define aval (first ($csp-vals csp aname)))
  (define (check-var var)
    (match var
      ;; don't check against assigned vars, or the reference var
      ;; (which is probably assigned but maybe not)
      [(? (λ (x) (or (assigned-var? x) (eq? (var-name x) aname)))) var]
      [($var name vals)
       (match ((constraints csp) . relating-only . (list aname name))
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
          ($cvar name new-vals (cons aname (if ($cvar? var)
                                               ($cvar-past var)
                                               null)))])]))
  (define checked-vars (map check-var (vars csp)))
  (when-debug (set! nfchecks (+ (length checked-vars) nchecks)))
  ;; conflict-set will be empty if there are no empty domains
  (define conflict-set (for*/list ([var (in-list checked-vars)]
                                   #:when (empty? ($var-domain var))
                                   [name (in-list ($cvar-past var))])
                         name))
  ;; for conflict-directed backjumping it's essential to forward-check ALL vars
  ;; (even after an empty domain is generated) and combine their conflicts
  ;; so we can discover the *most recent past var* that could be the culprit.
  ;; If we just bail out at the first conflict, we may backjump too far based on its history
  ;; (and thereby miss parts of the search tree)
  (when (pair? conflict-set)
    (backtrack! conflict-set))
  ;; Discard constraints that have produced singleton domains
  ;; (they have no further use)
  (define nonsingleton-constraints
    (for/list ([constraint (in-constraints csp)]
               #:unless (and
                         (binary-constraint? constraint)
                         (constraint-relates? constraint aname)
                         (let ([other-name (first (remq aname ($constraint-names constraint)))])
                           (singleton-var? (csp-var csp other-name)))))
      constraint))
  (make-csp checked-vars nonsingleton-constraints))

(define/contract (constraint-checkable? c names)
  ($constraint? (listof name?) . -> . any/c)
  ;; constraint is checkable if all constraint names
  ;; are in target list of names.
  (for/and ([cname (in-list ($constraint-names c))])
    (memq cname names)))

(define/contract (constraint-arity constraint)
  ($constraint? . -> . exact-nonnegative-integer?)
  (length ($constraint-names constraint)))

(define (singleton-var? var)
  (= 1 (length ($var-domain var))))

(define nchecks 0)
(define (reset-nchecks!) (set! nchecks 0))
(define/contract (check-constraints csp [mandatory-names #f] #:conflicts [conflict-count? #f])
  ((csp?) ((listof name?) #:conflicts boolean?) . ->* . (or/c csp? exact-nonnegative-integer?))
  ;; this time, we're not limited to assigned variables
  ;; (that is, vars that have been deliberately assigned in the backtrack process thus far)
  ;; we also want to use "singleton" vars (that is, vars that have been reduced to a single domain value by forward checking)
  (define singleton-varnames (for/list ([var (in-vars csp)]
                                        #:when (singleton-var? var))
                               (var-name var)))
  (define-values (checkable-constraints other-constraints)
    (partition (λ (c) (and (constraint-checkable? c singleton-varnames)
                           (or (not mandatory-names)
                               (for/and ([name (in-list mandatory-names)])
                                 (constraint-relates? c name)))))
               (constraints csp)))
  (cond
    [conflict-count? (define conflict-count
                       (for/sum ([constraint (in-list checkable-constraints)]
                                 #:unless (constraint csp))
                         1))
                     (when-debug (set! nchecks (+ conflict-count nchecks)))
                     conflict-count]
    [else (for ([(constraint idx) (in-indexed (sort checkable-constraints < #:key constraint-arity))]
                #:unless (constraint csp))
            (when-debug (set! nchecks (+ (add1 idx) nchecks)))
            (backtrack!))
          ;; discard checked constraints, since they have no further reason to live
          (make-csp (vars csp) other-constraints)]))

(define/contract (make-nodes-consistent csp)
  (csp? . -> . csp?)
  ;; todo: why does this function slow down searches?
  (make-csp
   (for/list ([var (in-vars csp)])
     (match-define ($var name vals) var)
     (define procs (for*/list ([constraint (in-constraints csp)]
                               [cnames (in-value ($constraint-names constraint))]
                               #:when (and (= 1 (length cnames)) (eq? name (car cnames))))
                     ($constraint-proc constraint)))
     ($var name
           (for*/fold ([vals vals])
                      ([proc (in-list procs)])
             (filter proc vals))))
   (constraints csp)))

(define/contract (backtracking-solver
                  csp
                  #:select-variable [select-unassigned-variable
                                     (or (current-select-variable) first-unassigned-variable)]
                  #:order-values [order-domain-values (or (current-order-values) first-domain-value)]
                  #:inference [inference (or (current-inference) no-inference)])
  ((csp?) (#:select-variable procedure? #:order-values procedure? #:inference procedure?) . ->* . generator?)
  (generator ()
             (let loop ([csp csp])
               (match (select-unassigned-variable csp)
                 [#false (yield csp)]
                 [($var name domain)
                  (define (wants-backtrack? exn)
                    (and ($backtrack? exn) (or (let ([btns ($backtrack-names exn)])
                                                 (or (empty? btns) (memq name btns))))))
                  (for/fold ([conflicts null]
                             #:result (void))
                            ([val (in-list (order-domain-values domain))])
                    (with-handlers ([wants-backtrack?
                                     (λ (bt) (append conflicts (remq name ($backtrack-names bt))))])
                      (let* ([csp (assign-val csp name val)]
                             ;; reduce constraints before inference,
                             ;; to create more forward-checkable (binary) constraints
                             [csp (reduce-constraint-arity csp)]
                             [csp (inference csp name)]
                             [csp (check-constraints csp)])
                        (loop csp)))
                    conflicts)]))))

(define (random-pick xs)
  (list-ref xs (random (length xs))))

(define (assign-random-vals csp)
  (for/fold ([new-csp csp])
            ([name (in-var-names csp)])
    (assign-val new-csp name (random-pick ($csp-vals csp name)))))

(define (make-min-conflcts-thread csp0 thread-count max-steps [main-thread (current-thread)])
  (thread
   (λ ()
     (let loop ()
       ;; Generate a complete assignment for all variables (probably with conflicts)
       (for/fold ([csp (assign-random-vals csp0)])
                 ([nth-step (in-range max-steps)])
         ;; Now repeatedly choose a random conflicted variable and change it
         (match (conflicted-var-names csp)
           [(? empty?) (thread-send main-thread csp) (loop)]
           [names
            (define name (random-pick names))
            (define val (min-conflicts-value csp name ($csp-vals csp0 name)))
            (assign-val csp name val)]))))))

(define/contract (min-conflicts-solver csp [max-steps 100])
  (($csp?) (integer?) . ->* . generator?)
  ;; Solve a CSP by stochastic hillclimbing on the number of conflicts.  
  (generator ()
             (for ([thread-count (if (current-multithreaded) 4 1)]) ; todo: what is ideal thread count?
               (make-min-conflcts-thread csp thread-count max-steps))
             (for ([i (in-naturals)])
               (yield (thread-receive)))))

(define/contract (conflicted-var-names csp)
  ($csp? . -> . (listof name?))
  ;; Return a list of variables in current assignment that are conflicted
  (for/list ([name (in-var-names csp)]
             #:when (positive? (nconflicts csp name)))
    name))

(define/contract (optimal-stop-min proc xs)
  (procedure? (listof any/c) . -> . any/c)
  (define-values (sample candidates) (split-at xs (inexact->exact (floor (* .458 (length xs))))))
  (define threshold (argmin proc sample))
  (or (for/first ([c (in-list candidates)]
                  #:when (<= (proc c) threshold))
        c)
      (last candidates)))
  
 
(define/contract (min-conflicts-value csp name vals)
  ($csp? name? (listof any/c) . -> . any/c)
  ;; Return the value that will give var the least number of conflicts
  #;(optimal-stop-min (λ (val) (nconflicts csp name val)) vals)
  (define vals-by-conflict (sort vals < #:key (λ (val) (nconflicts csp name val))
                                 #:cache-keys? #true))
  (for/first ([val (in-list vals-by-conflict)]
              #:unless (equal? val (first ($csp-vals csp name)))) ;; but change the value
    val))

(define no-value-sig (gensym))

(define/contract (nconflicts csp name [val no-value-sig])
  (($csp? name?) (any/c) . ->* . exact-nonnegative-integer?)
  ;; How many conflicts var: val assignment has with other variables.
  (check-constraints (if (eq? val no-value-sig)
                         csp
                         (assign-val csp name val)) (list name) #:conflicts #true))

(define/contract (csp->assocs csp)
  (csp? . -> . (listof (cons/c name? any/c)))
  (for/list ([var (in-vars csp)])
    (match var
      [($var name (list val)) (cons name val)])))

(define/contract (combine-csps csps)
  ((listof $csp?) . -> . $csp?)
  (make-csp
   (apply append (map $csp-vars csps))
   (apply append (map $csp-constraints csps))))

(define/contract (make-cartesian-generator solgens)
  ((listof generator?) . -> . generator?)
  (generator ()
             (define solstreams (for/list ([solgen (in-list solgens)])
                                  (for/stream ([sol (in-producer solgen (void))]) 
                                    sol))) 
             (let loop ([solstreams solstreams][sols empty])
               (if (null? solstreams)
                   (yield (combine-csps (reverse sols)))
                   (for ([sol (in-stream (car solstreams))])
                     (loop (cdr solstreams) (cons sol sols)))))))

(define/contract (extract-subcsp csp names)
  ($csp? (listof name?) . -> . $csp?)
  (make-csp
   (for/list ([var (in-vars csp)]
              #:when (memq (var-name var) names))
     var)
   (for/list ([constraint (in-constraints csp)]
              #:when (for/and ([cname (in-list ($constraint-names constraint))])
                       (memq cname names)))
     constraint)))

(define/contract (solve* csp
                         #:finish-proc [finish-proc csp->assocs]
                         #:solver [solver (or (current-solver) backtracking-solver)]
                         #:limit [max-solutions +inf.0])
  ((csp?) (#:finish-proc procedure? #:solver procedure? #:limit exact-nonnegative-integer?)
          . ->* . (listof any/c))
  (when-debug (reset-assns!) (reset-nfcs!) (reset-nchecks!))          

  (define subcsps ; decompose into independent csps. `cc` determines "connected components"
    (if (current-decompose)
        (for/list ([nodeset (in-list (cc (csp->graph csp)))])
          (extract-subcsp csp nodeset))
        (list csp)))
  
  (for/list ([solution (in-producer (make-cartesian-generator (map solver subcsps)) (void))]
             [idx (in-range max-solutions)])
    (finish-proc solution)))

(define/contract (solve csp
                        #:finish-proc [finish-proc csp->assocs]
                        #:solver [solver (or (current-solver) backtracking-solver)]
                        #:limit [max-solutions 1])
  ((csp?) (#:finish-proc procedure? #:solver procedure? #:limit exact-nonnegative-integer?)
          . ->* . (or/c #false any/c))
  (match (solve* csp #:finish-proc finish-proc #:solver solver #:limit max-solutions)
    [(list solution) solution]
    [(list solutions ...) solutions]
    [else #false]))

(define (<> a b) (not (= a b)))
(define (neq? a b) (not (eq? a b)))

