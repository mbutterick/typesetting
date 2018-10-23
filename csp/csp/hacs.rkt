#lang debug racket
(require racket/generator graph)
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
                        (for ([arg (car argss)])
                          (loop (cdr argss) (cons arg acc))))))))

(struct csp (vars constraints [assignments #:auto] [checks #:auto])
  #:mutable #:transparent #:auto-value 0)
(define constraints csp-constraints)
(define vars csp-vars)
(define-syntax-rule (in-constraints csp) (in-list (csp-constraints csp)))
(define-syntax-rule (in-vars csp) (in-list (vars csp)))
(define-syntax-rule (in-variable-names csp) (in-list (map var-name (vars csp))))

(struct constraint (names proc) #:transparent
  #:property prop:procedure
  (λ (const prob)
    (unless (csp? prob)
      (raise-argument-error 'constraint-proc "csp" prob))
    ;; apply proc in many-to-many style
    (for/and ([args (in-cartesian (map (λ (name) (find-domain prob name)) (constraint-names const)))])
      (apply (constraint-proc const) args))))

(define name? symbol?)

(define/contract (make-constraint [names null] [proc values])
  (() ((listof name?) procedure?) . ->* . constraint?)
  (constraint names proc))

(define/contract (csp->graphviz prob)
  (csp? . -> . string?)
  (define g (csp->graph prob))
  (graphviz g #:colors (coloring/brelaz g)))

(define/contract (csp->graph prob)
  (csp? . -> . graph?)
  (for*/fold ([gr (unweighted-graph/undirected (map var-name (vars prob)))])
             ([constraint (in-constraints prob)]
              [edge (in-combinations (constraint-names constraint) 2)])
    (apply add-edge! gr edge)
    gr))

(struct var (name domain) #:transparent)
(define domain var-domain)

(struct checked-variable var (history) #:transparent)
(define history checked-variable-history)
(define cvar checked-variable)
(define cvar? checked-variable?)

(struct assigned-var var () #:transparent)
(define avar assigned-var)
(define avar? assigned-var?)

(define/contract (make-csp [vars null] [consts null])
  (() ((listof var?) (listof constraint?)) . ->* . csp?)
  (csp vars consts))

(define/contract (add-vars! prob names-or-procedure [vals-or-procedure empty])
  ((csp? (or/c (listof name?) procedure?)) ((or/c (listof any/c) procedure?)) . ->* . void?)
  (for/fold ([vrs (vars prob)]
             #:result (set-csp-vars! prob vrs))
            ([name (in-list (match names-or-procedure
                              [(? procedure? proc) (proc)]
                              [names names]))])
    (when (memq name (map var-name vrs))
      (raise-argument-error 'add-vars! "var that doesn't already exist" name))
    (append vrs (list (var name
                           (if (procedure? vals-or-procedure)
                               (vals-or-procedure)
                               vals-or-procedure))))))

(define/contract (add-var! prob name [vals-or-procedure empty])
  ((csp? name?) ((or/c (listof any/c) procedure?)) . ->* . void?)
  (add-vars! prob (list name) vals-or-procedure))

(define/contract (add-constraints! prob proc namess [proc-name #false])
  ((csp? procedure? (listof (listof name?))) ((or/c #false name?)) . ->* . void?)
  (set-csp-constraints! prob (append (constraints prob) 
                                     (for/list ([names (in-list namess)])
                                       (for ([name (in-list names)])
                                         (check-name-in-csp! 'add-constraints! prob name))
                                       (make-constraint names (if proc-name
                                                                  (procedure-rename proc proc-name)
                                                                  proc))))))

(define/contract (add-pairwise-constraint! prob proc var-names [proc-name #false])
  ((csp? procedure? (listof name?)) (name?) . ->* . void?)
  (add-constraints! prob proc (combinations var-names 2) proc-name))

(define/contract (add-constraint! prob proc names [proc-name #false])
  ((csp? procedure? (listof name?)) (name?) . ->* . void?)
  (add-constraints! prob proc (list names) proc-name))

(define/contract (alldiff= x y)
  (any/c any/c . -> . boolean?)
  (not (= x y)))

(struct backtrack (names) #:transparent)
(define (backtrack! [names null]) (raise (backtrack names)))

(define current-select-variable (make-parameter #f))
(define current-order-values (make-parameter #f))
(define current-inference (make-parameter #f))
(define current-solver (make-parameter #f))
(define current-random (make-parameter #t))
(define current-decompose (make-parameter #t))
(define current-thread-count (make-parameter 4))

(define/contract (check-name-in-csp! caller prob name)
  (symbol? csp? name? . -> . void?)
  (define names (map var-name (vars prob)))
  (unless (memq name names)
    (raise-argument-error caller (format "one of these existing csp var names: ~v" names) name)))

(define/contract (find-var prob name)
  (csp? name? . -> . var?)
  (check-name-in-csp! 'find-var prob name)
  (for/first ([vr (in-vars prob)]
              #:when (eq? name (var-name vr)))
    vr))

(define/contract (find-domain prob name)
  (csp? name? . -> . (listof any/c))
  (check-name-in-csp! 'find-domain prob name)
  (domain (find-var prob name)))

(define order-domain-values values)

(define/contract (assigned-name? prob name)
  (csp? name? . -> . any/c)
  (for/or ([vr (in-vars prob)]
           #:when (assigned-var? vr))
    (eq? name (var-name vr))))

(define/contract (reduce-function-arity proc pattern)
  (procedure? (listof any/c) . -> . procedure?)
  (unless (match (procedure-arity proc)
            [(arity-at-least val) (<= val (length pattern))]
            [(? number? val) (= val (length pattern))])
    (raise-argument-error 'reduce-function-arity (format "list of length ~a, same as procedure arity" (procedure-arity proc)) pattern))
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

(define/contract (reduce-constraint-arity prob [minimum-arity 3])
  ((csp?) ((or/c #false natural?)) . ->* . csp?)
  (define assigned? (curry assigned-name? prob))
  (define (partially-assigned? constraint)
    (ormap assigned? (constraint-names constraint)))
  (make-csp (vars prob)
            (for/list ([const (in-constraints prob)])
              (cond
                ;; no point reducing 2-arity functions because they will be consumed by forward checking
                [(and (or (not minimum-arity) (<= minimum-arity (constraint-arity const)))
                      (partially-assigned? const))
                 (match-define (constraint cnames proc) const)
                 ;; pattern is mix of values and boxed symbols (indicating variables to persist)
                 ;; use boxes here as cheap way to distinguish id symbols from value symbols
                 (define reduce-arity-pattern (for/list ([cname (in-list cnames)])
                                                (if (assigned? cname)
                                                    (first (find-domain prob cname))
                                                    (box cname))))
                 (constraint (filter-not assigned? cnames)
                             (reduce-function-arity proc reduce-arity-pattern))]
                [else const]))))

(define nassns 0)

(define (reset-assns!) (set! nassns 0))

(define nfchecks 0)

(define (reset-nfcs!) (set! nfchecks 0))

(define nchecks 0)

(define (reset-nchecks!) (set! nchecks 0))

(define/contract (assign-val prob name val)
  (csp? name? any/c . -> . csp?)
  (when-debug (set! nassns (add1 nassns)))
  (make-csp
   (for/list ([vr (in-vars prob)])
     (if (eq? name (var-name vr))
         (assigned-var name (list val))
         vr))
   (constraints prob)))

(define/contract (unassigned-vars prob)
  (csp? . -> . (listof (and/c var? (not/c assigned-var?))))
  (filter-not assigned-var? (vars prob)))

(define/contract (first-unassigned-variable csp)
  (csp? . -> . (or/c #false (and/c var? (not/c assigned-var?))))
  (match (unassigned-vars csp)
    [(? empty?) #false]
    [xs (first xs)]))

(define/contract (minimum-remaining-values prob)
  (csp? . -> . (or/c #false (and/c var? (not/c assigned-var?))))
  (match (unassigned-vars prob)
    [(? empty?) #false]
    [xs (argmin (λ (var) (length (domain var))) xs)]))

(define mrv minimum-remaining-values)

(define/contract (var-degree prob var)
  (csp? var? . -> . natural?)
  (for/sum ([const (in-constraints prob)]
            #:when (memq (var-name var) (constraint-names const)))
    1))

(define/contract (domain-length var)
  (var? . -> . natural?)
  (length (domain var)))

(define/contract (mrv-degree-hybrid prob)
  (csp? . -> . (or/c #f var?))
  (match (unassigned-vars prob)
    [(? empty?) #false]
    [uvars
     ;; minimum remaining values (MRV) rule
     (define mrv-arg (argmin domain-length uvars))
     (match (filter (λ (var) (= (domain-length mrv-arg) (domain-length var))) uvars)
       [(list winning-uvar) winning-uvar] 
       [(list mrv-uvars ...)
        ;; use degree as tiebreaker for mrv
        (define degrees (map (λ (var) (var-degree prob var)) mrv-uvars))
        (define max-degree (apply max degrees))
        ;; use random tiebreaker for degree
        (random-pick (for/list ([uv (in-list mrv-uvars)]
                                [degree (in-list degrees)]
                                #:when (= max-degree degree))
                       uv))])]))
  
(define first-domain-value values)

(define (no-inference prob name) prob)

(define/contract (relating-only constraints names)
  ((listof constraint?) (listof name?) . -> . (listof constraint?))
  (for*/list ([const (in-list constraints)]
              [cnames (in-value (constraint-names const))]
              #:when (and (= (length names) (length cnames))
                          (for/and ([name (in-list names)])
                            (memq name cnames))))
    const))

(define (two-arity? const) (= 2 (constraint-arity const)))

(define (constraint-relates? const name)
  (memq name (constraint-names const)))

(define/contract (forward-check prob ref-name)
  (csp? name? . -> . csp?)
  (define aval (first (find-domain prob ref-name)))
  (define (check-var vr)
    (match vr
      ;; don't check against assigned vars, or the reference var
      ;; (which is probably assigned but maybe not)
      [(? (λ (x) (or (assigned-var? x) (eq? (var-name x) ref-name)))) vr]
      [(var name vals)
       (match ((constraints prob) . relating-only . (list ref-name name))
         [(? empty?) vr]
         [constraints
          (define new-vals
            (for/list ([val (in-list vals)]
                       #:when (for/and ([const (in-list constraints)])
                                (let ([proc (constraint-proc const)])
                                  (if (eq? name (first (constraint-names const)))
                                      (proc val aval)
                                      (proc aval val)))))
              val))
          (checked-variable name new-vals (cons ref-name (match vr
                                                           [(checked-variable _ _ history) history]
                                                           [else null])))])]))
  (define checked-vars (map check-var (vars prob)))
  (when-debug (set! nfchecks (+ (length checked-vars) nchecks)))
  ;; conflict-set will be empty if there are no empty domains (as we would hope)
  (define conflict-set (for*/list ([cvr (in-list checked-vars)]
                                   #:when (empty? (domain cvr))
                                   [name (in-list (history cvr))])
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
    (for/list ([const (in-constraints prob)]
               #:unless (and (two-arity? const)
                             (constraint-relates? const ref-name)
                             (let ([other-name (first (remq ref-name (constraint-names const)))])
                               (singleton-var? (find-var prob other-name)))))
      const))
  (make-csp checked-vars nonsingleton-constraints))

(define/contract (constraint-checkable? const names)
  (constraint? (listof name?) . -> . any/c)
  ;; constraint is checkable if all constraint names
  ;; are in target list of names.
  (for/and ([cname (in-list (constraint-names const))])
    (memq cname names)))

(define/contract (constraint-arity const)
  (constraint? . -> . natural?)
  (length (constraint-names const)))

(define (singleton-var? var)
  (= 1 (domain-length var)))

(define/contract (check-constraints prob [mandatory-names #f] #:conflicts [conflict-count? #f])
  ((csp?) ((listof name?) #:conflicts boolean?) . ->* . (or/c csp? natural?))
  ;; this time, we're not limited to assigned variables
  ;; (that is, vars that have been deliberately assigned in the backtrack process thus far)
  ;; we also want to use "singleton" vars (that is, vars that have been reduced to a single domain value by forward checking)
  (define singleton-varnames (for/list ([vr (in-vars prob)]
                                        #:when (singleton-var? vr))
                               (var-name vr)))
  (define-values (checkable-consts other-consts)
    (partition (λ (const) (and (constraint-checkable? const singleton-varnames)
                               (or (not mandatory-names)
                                   (for/and ([name (in-list mandatory-names)])
                                     (constraint-relates? const name)))))
               (constraints prob)))
  (cond
    [conflict-count?
     (define conflict-count
       (for/sum ([constraint (in-list checkable-consts)]
                 #:unless (constraint prob))
         1))
     (when-debug (set! nchecks (+ conflict-count nchecks)))
     conflict-count]
    [else
     (for ([(constraint idx) (in-indexed checkable-consts)]
           #:unless (constraint prob))
       (when-debug (set! nchecks (+ (add1 idx) nchecks)))
       (backtrack!))
     ;; discard checked constraints, since they have no further reason to live
     (make-csp (vars prob) other-consts)]))

(define/contract (make-nodes-consistent prob)
  (csp? . -> . csp?)
  ;; todo: why does this function slow down searches?
  (make-csp
   (for/list ([vr (in-vars prob)])
     (match-define (var name vals) vr)
     (define procs (for*/list ([const (in-constraints prob)]
                               [cnames (in-value (constraint-names const))]
                               #:when (and (= 1 (length cnames)) (eq? name (car cnames))))
                     (constraint-proc const)))
     (var name
          (for*/fold ([vals vals])
                     ([proc (in-list procs)])
            (filter proc vals))))
   (constraints prob)))

(define/contract (backtracking-solver
                  prob
                  #:select-variable [select-unassigned-variable
                                     (or (current-select-variable) first-unassigned-variable)]
                  #:order-values [order-domain-values (or (current-order-values) first-domain-value)]
                  #:inference [inference (or (current-inference) no-inference)])
  ((csp?) (#:select-variable procedure? #:order-values procedure? #:inference procedure?) . ->* . generator?)
  (generator ()
             (let loop ([prob prob])
               (match (select-unassigned-variable prob)
                 [#false (yield prob)]
                 [(var name domain)
                  (define (wants-backtrack? exn)
                    (and (backtrack? exn) (or (let ([btns (backtrack-names exn)])
                                                (or (empty? btns) (memq name btns))))))
                  (for/fold ([conflicts null]
                             #:result (void))
                            ([val (in-list (order-domain-values domain))])
                    (with-handlers ([wants-backtrack?
                                     (λ (bt) (append conflicts (remq name (backtrack-names bt))))])
                      (let* ([prob (assign-val prob name val)]
                             ;; reduce constraints before inference,
                             ;; to create more forward-checkable (binary) constraints
                             [prob (reduce-constraint-arity prob)]
                             [prob (inference prob name)]
                             [prob (check-constraints prob)])
                        (loop prob)))
                    conflicts)]))))

(define (random-pick xs)
  (list-ref xs (random (length xs))))

(define (assign-random-vals prob)
  (for/fold ([new-csp prob])
            ([name (in-variable-names prob)])
    (assign-val new-csp name (random-pick (find-domain prob name)))))

(define (make-min-conflcts-thread prob-start thread-count max-steps [main-thread (current-thread)])
  (thread
   (λ ()
     (let loop ()
       ;; Generate a complete assignment for all variables (probably with conflicts)
       (for/fold ([prob (assign-random-vals prob-start)])
                 ([nth-step (in-range max-steps)])
         ;; Now repeatedly choose a random conflicted variable and change it
         (match (conflicted-variable-names prob)
           [(? empty?) (thread-send main-thread prob) (loop)]
           [names
            (define name (random-pick names))
            (define val (min-conflicts-value prob name (find-domain prob-start name)))
            (assign-val prob name val)]))))))

(define/contract (min-conflicts-solver prob [max-steps 100])
  ((csp?) (integer?) . ->* . generator?)
  (generator ()
             (for ([thread-count (or (current-thread-count) 1)]) ; todo: what is ideal thread count?
               (make-min-conflcts-thread prob thread-count max-steps))
             (for ([i (in-naturals)])
               (yield (thread-receive)))))

(define/contract (optimal-stop-min proc xs)
  (procedure? (listof any/c) . -> . any/c)
  (define-values (sample candidates) (split-at xs (inexact->exact (floor (* .458 (length xs))))))
  (define threshold (argmin proc sample))
  (or (for/first ([candidate (in-list candidates)]
                  #:when (<= (proc candidate) threshold))
        candidate)
      (last candidates)))

(define/contract (conflicted-variable-names prob)
  (csp? . -> . (listof name?))
  ;; Return a list of variables in current assignment that are conflicted
  (for/list ([name (in-variable-names prob)]
             #:when (positive? (nconflicts prob name)))
    name))
 
(define/contract (min-conflicts-value prob name vals)
  (csp? name? (listof any/c) . -> . any/c)
  ;; Return the value that will give var the least number of conflicts
  (define vals-by-conflict (sort vals < #:key (λ (val) (nconflicts prob name val))
                                 #:cache-keys? #true))
  (for/first ([val (in-list vals-by-conflict)]
              #:unless (equal? val (first (find-domain prob name)))) ;; but change the value
    val))

(define no-value-sig (gensym))

(define/contract (nconflicts prob name [val no-value-sig])
  ((csp? name?) (any/c) . ->* . natural?)
  ;; How many conflicts var: val assignment has with other variables.
  (check-constraints (if (eq? val no-value-sig)
                         prob
                         (assign-val prob name val)) (list name) #:conflicts #true))

(define/contract (csp->assocs prob)
  (csp? . -> . (listof (cons/c name? any/c)))
  (for/list ([vr (in-vars prob)])
    (match vr
      [(var name (list val)) (cons name val)])))

(define/contract (combine-csps probs)
  ((listof csp?) . -> . csp?)
  (make-csp
   (apply append (map vars probs))
   (apply append (map csp-constraints probs))))


(define/contract (extract-subcsp prob names)
  (csp? (listof name?) . -> . csp?)
  (make-csp
   (for/list ([vr (in-vars prob)]
              #:when (memq (var-name vr) names))
     vr)
   (for/list ([const (in-constraints prob)]
              #:when (for/and ([cname (in-list (constraint-names const))])
                       (memq cname names)))
     const)))

(define/contract (solve* prob
                         #:finish-proc [finish-proc csp->assocs]
                         #:solver [solver (or (current-solver) backtracking-solver)]
                         #:limit [max-solutions +inf.0])
  ((csp?) (#:finish-proc procedure? #:solver procedure? #:limit natural?)
          . ->* . (listof any/c))
  (when-debug (reset-assns!) (reset-nfcs!) (reset-nchecks!))          

  (define subcsps ; decompose into independent csps. `cc` determines "connected components"
    (if (current-decompose)
        (for/list ([nodeset (in-list (cc (csp->graph prob)))])
          (extract-subcsp prob nodeset))
        (list prob)))

  (define solgens (map solver subcsps))
  (define solstreams (for/list ([solgen (in-list solgens)])
                       (for/stream ([sol (in-producer solgen (void))]) 
                         sol)))
  
  (for/list ([solution-pieces (in-cartesian solstreams)]
             [idx (in-range max-solutions)])
    (finish-proc (combine-csps solution-pieces))))

(define/contract (solve prob
                        #:finish-proc [finish-proc csp->assocs]
                        #:solver [solver (or (current-solver) backtracking-solver)]
                        #:limit [max-solutions 1])
  ((csp?) (#:finish-proc procedure? #:solver procedure? #:limit natural?)
          . ->* . (or/c #false any/c))
  (match (solve* prob #:finish-proc finish-proc #:solver solver #:limit max-solutions)
    [(list solution) solution]
    [(list solutions ...) solutions]
    [else #false]))

(define (<> a b) (not (= a b)))
(define (neq? a b) (not (eq? a b)))

