#lang debug racket
(require racket/generator graph racket/set)
(provide (except-out (all-defined-out) define/contract))

(define-syntax-rule (define/contract EXPR CONTRACT . BODY)
  (define EXPR . BODY))

(define-syntax when-debug
  (let ()
    (define debug #t)
    (if debug
        (make-rename-transformer #'begin)
        (λ (stx) (syntax-case stx ()
                   [(_ . rest) #'(void)])))))

(define (print-debug-info)
  (when-debug
   (displayln (format "assignments: ~a  forward checks: ~a  checks: ~a " nassns nchecks nfchecks))))

(define-syntax-rule (in-cartesian x)
  (in-generator (let ([argss x])
                  (let loop ([argss argss][acc empty])
                    (if (null? argss)
                        (yield (reverse acc))
                        (for ([arg (in-stream (car argss))])
                          (loop (cdr argss) (cons arg acc))))))))

(struct csp (vars constraints) #:mutable #:transparent)
(define constraints csp-constraints)
(define vars csp-vars)
(define-syntax-rule (in-constraints csp) (in-list (csp-constraints csp)))
(define-syntax-rule (in-vars csp) (in-list (vars csp)))
(define-syntax-rule (in-var-names csp) (in-list (map var-name (vars csp))))

(struct constraint (names proc) #:transparent
  #:property prop:procedure
  (λ (const prob)
    (unless (csp? prob)
      (raise-argument-error 'constraint "csp" prob))
    ;; apply proc in many-to-many style
    (for/and ([args (in-cartesian (map (λ (name) (find-domain prob name)) (constraint-names const)))])
      (apply (constraint-proc const) args))))

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
(define (var-name? x) #true) ; anything is ok for now
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

(define/contract (make-var name [vals null])
  ((name?) ((listof any/c)) . ->* . var?)
  (var name (list->set vals)))

(define/contract (make-var-names prefix vals [suffix ""])
  ((string? (listof any/c)) ((string?)) . ->* . (listof name?))
  (for/list ([val (in-list vals)])
    (string->symbol (format "~a~a~a" prefix val suffix))))

(define/contract (add-vars! prob names [vals-or-procedure empty])
  ((csp? (listof name?)) ((or/c (listof any/c) procedure?)) . ->* . void?)
  (for/fold ([vrs (vars prob)]
             #:result (set-csp-vars! prob vrs))
            ([name (in-list names)])
    (when (memq name (map var-name vrs))
      (raise-argument-error 'add-vars! "var that doesn't already exist" name))
    (append  vrs (list (make-var name
                                 (match vals-or-procedure
                                   [(? procedure? proc) (proc)]
                                   [vals vals]))))))

(define/contract (add-var! prob name [vals-or-procedure empty])
  ((csp? name?) ((or/c (listof any/c) procedure?)) . ->* . void?)
  (add-vars! prob (list name) vals-or-procedure))

(define/contract (add-constraints! prob proc namess [proc-name #false]
                                   #:caller [caller-id 'add-constraints!])
  ((csp? procedure? (listof (listof name?))) ((or/c #false name?)) . ->* . void?)
  (unless (procedure? proc)
    (raise-argument-error caller-id "procedure" proc))
  (unless (and (list? namess) (andmap list? namess))
    (raise-argument-error caller-id "list of lists of names" namess))
  (set-csp-constraints! prob (append (constraints prob) 
                                     (for/list ([names (in-list namess)])
                                       (for ([name (in-list names)])
                                         (check-name-in-csp! 'add-constraints! prob name))
                                       (make-constraint names (if proc-name
                                                                  (procedure-rename proc proc-name)
                                                                  proc))))))

(define/contract (add-pairwise-constraint! prob proc names [proc-name #false])
  ((csp? procedure? (listof name?)) (name?) . ->* . void?)
  (unless (list? names)
    (raise-argument-error 'add-pairwise-constraint! "list of names" names))
  (add-constraints! prob proc (combinations names 2) proc-name #:caller 'add-pairwise-constraint!))

(define/contract (add-constraint! prob proc names [proc-name #false])
  ((csp? procedure? (listof name?)) (name?) . ->* . void?)
  (unless (list? names)
    (raise-argument-error 'add-constraint! "list of names" names))
  (add-constraints! prob proc (list names) proc-name #:caller 'add-constraint!))

(define/contract (alldiff x y)
  (any/c any/c . -> . boolean?)
  (not (= x y)))
(define alldiff= alldiff)

(define (add-all-diff-constraint! prob [names (map var-name (csp-vars prob))]
                                  #:proc [equal-proc equal?])
  (add-pairwise-constraint! prob (λ (x y) (not (equal-proc x y))) names
                            (string->symbol (format "all-diff-~a" (object-name equal-proc)))))

(struct backtrack (histories) #:transparent)
(define (backtrack! [names null]) (raise (backtrack names)))

(define current-select-variable (make-parameter #f))
(define current-order-values (make-parameter #f))
(define current-inference (make-parameter #f))
(define current-solver (make-parameter #f))
(define current-random (make-parameter #t))
(define current-decompose (make-parameter #t))
(define current-thread-count (make-parameter 4))
(define current-node-consistency (make-parameter #f))
(define current-arity-reduction (make-parameter #t))
(define current-learning (make-parameter #f))

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
  (assigned-var? (find-var prob name)))

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
                 (define arity-reduction-pattern (for/list ([cname (in-list cnames)])
                                                   (if (assigned? cname)
                                                       (first (find-domain prob cname))
                                                       (box cname))))
                 (constraint (filter-not assigned? cnames)
                             (reduce-function-arity proc arity-reduction-pattern))]
                [else const]))))

(define nassns 0)
(define nfchecks 0)
(define nchecks 0)

(define (reset-nassns!) (set! nassns 0))
(define (reset-nfchecks!) (set! nfchecks 0))
(define (reset-nchecks!) (set! nchecks 0))

(define/contract (assign-val prob name val)
  (csp? name? any/c . -> . csp?)
  (begin0
    (make-csp
     (for/list ([vr (in-vars prob)])
       (if (eq? name (var-name vr))
           (assigned-var name (list val))
           vr))
     (constraints prob))
    (when-debug (set! nassns (add1 nassns)))))

(define/contract (assigned-vars prob [invert? #f])
  ((csp?) (any/c) . ->* . (listof var?))
  ((if invert? filter-not filter) assigned-var? (vars prob)))

(define/contract (unassigned-vars prob)
  (csp? . -> . (listof var?))
  (assigned-vars prob 'invert))

(define/contract (first-unassigned-variable csp)
  (csp? . -> . (or/c #false (and/c var? (not/c assigned-var?))))
  (match (unassigned-vars csp)
    [(== empty) #false]
    [uvars (first uvars)]))

(define/contract (argmin* proc xs [max-style? #f])
  ((procedure? (listof any/c)) (any/c) . ->* . (listof any/c))
  ;; return all elements that have min value.
  (match xs
    [(== empty) xs]
    [(list x) xs]
    [xs
     (define vals (map proc xs))
     (define target-val (apply (if max-style? max min) vals))
     (for/list ([x (in-list xs)]
                [val (in-list vals)]
                #:when (= val target-val))
       x)]))

(define/contract (argmax* proc xs)
  (procedure? (listof any/c) . -> . (listof any/c))
  ;; return all elements that have max value.
  (argmin* proc xs 'max-mode!))

(define/contract (minimum-remaining-values prob)
  (csp? . -> . (or/c #false (and/c var? (not/c assigned-var?))))
  (match (unassigned-vars prob)
    [(== empty) #false]
    [uvars (random-pick (argmin* domain-length uvars))]))

(define/contract (max-degree prob)
  (csp? . -> . (or/c #false (and/c var? (not/c assigned-var?))))
  (match (unassigned-vars prob)
    [(== empty) #false]
    [uvars (random-pick (argmax* (λ (var) (var-degree prob var)) uvars))]))

(define mrv minimum-remaining-values)

(define/contract (var-degree prob var)
  (csp? var? . -> . natural?)
  (for/sum ([const (in-constraints prob)]
            #:when (memq (var-name var) (constraint-names const)))
    1))

(define/contract (domain-length var)
  (var? . -> . natural?)
  (set-count (domain var)))

(define/contract (state-count csp)
  (csp? . -> . natural?)
  (for/product ([vr (in-vars csp)])
    (domain-length vr)))

(define/contract (mrv-degree-hybrid prob)
  (csp? . -> . (or/c #f var?))
  (match (unassigned-vars prob)
    [(== empty) #false]
    [uvars
     (max-degree (make-csp (argmin* domain-length uvars) (constraints prob)))]))
  
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

(define (one-arity? const) (= 1 (constraint-arity const)))
(define (two-arity? const) (= 2 (constraint-arity const)))

(define (constraint-relates? const name)
  (memq name (constraint-names const)))

(struct arc (name const) #:transparent)

(define/contract (two-arity-constraints->arcs constraints)
  ((listof (and/c constraint? two-arity?)) . -> . (listof arc?))
  (for*/list ([const (in-list constraints)]
              [name (in-list (constraint-names const))])
    (arc name const)))

(require sugar/debug)
(define/contract (reduce-domain prob ark)
  (csp? arc? . -> . csp?)
  (match-define (arc name (constraint names constraint-proc)) ark)
  (match-define (list other-name) (remove name names))
  (define proc (if (eq? name (first names)) ; name is on left
                   constraint-proc ; so val stays on left
                   (λ (val other-val) (constraint-proc other-val val)))) ; otherwise reverse arg order
  (define (satisfies-arc? val)
    (for/or ([other-val (in-list (find-domain prob other-name))])
      (proc val other-val)))
  (make-csp
   (for/list ([vr (in-vars prob)])
     (cond
       [(assigned-var? vr) vr]
       [(eq? name (var-name vr))
        (make-var name (match (filter satisfies-arc? (domain vr))
                         [(? empty?) (backtrack!)]
                         [vals vals]))]
       [else vr]))
   (constraints prob)))

(define/contract (terminating-at? arcs name)
  ((listof arc?) name? . -> . (listof arc?))
  (for/list ([arc (in-list arcs)]
             #:when (and
                     (memq name (constraint-names (arc-const arc)))
                     (not (eq? name (arc-name arc)))))
    arc))

(define/contract (ac-3 prob ref-name)
  (csp? name? . -> . csp?)
  ;; csp is arc-consistent if every pair of variables (x y)
  ;; has values in their domain that satisfy every binary constraint
  (define checkable-names (cons ref-name (filter-not (λ (vn) (assigned-name? prob vn)) (map var-name (vars prob)))))
  (define starting-arcs
    (two-arity-constraints->arcs (for/list ([const (in-constraints prob)]
                                            #:when (and (two-arity? const)
                                                        (for/and ([cname (in-list (constraint-names const))])
                                                          (memq cname checkable-names))))
                                   const)))
  (for/fold ([prob prob]
             [arcs (sort starting-arcs < #:key (λ (a) (length (find-domain prob (arc-name a)))) #:cache-keys? #true)]
             #:result (prune-singleton-constraints prob))
            ([i (in-naturals)]
             #:break (empty? arcs))
    (match-define (cons (arc name proc) other-arcs) arcs)
    (define reduced-csp (reduce-domain prob (arc name proc)))
    (define (domain-reduced? name)
      (= (length (find-domain prob name)) (length (find-domain reduced-csp name))))
    (values reduced-csp (if (domain-reduced? name)
                            (remove-duplicates (append (starting-arcs . terminating-at? . name) other-arcs))
                            other-arcs))))


(define/contract (forward-check-var prob ref-name vr)
  (csp? name? var? . -> . var?)
  (match vr
    ;; don't check against assigned vars, or the reference var
    ;; (which is probably assigned but maybe not)
    [(? assigned-var? vr) vr]
    [(var (== ref-name eq?) _) vr]
    [(var name vals)
     (match ((constraints prob) . relating-only . (list ref-name name))
       [(? empty?) vr]
       [constraints
        (define ref-val (first (find-domain prob ref-name)))
        (define new-vals
          (for/set ([val (in-set vals)]
                    #:when (for/and ([const (in-list constraints)])
                             (match const
                               [(constraint (list (== name eq?) _) proc) (proc val ref-val)]
                               [(constraint _ proc) (proc ref-val val)])))
            val))
        (checked-variable name new-vals (cons (cons ref-name ref-val) (match vr
                                                                        [(checked-variable _ _ history) history]
                                                                        [_ null])))])]))

(define/contract (prune-singleton-constraints prob [ref-name #false])
  ((csp?) ((or/c #false name?)) . ->* . csp?)
  (define singleton-var-names (for/list ([vr (in-vars prob)]
                                         #:when (singleton-var? vr))
                                (var-name vr)))
  (make-csp
   (vars prob)
   (for/list ([const (in-constraints prob)]
              #:unless (and (two-arity? const)
                            (or (not ref-name) (constraint-relates? const ref-name))
                            (for/and ([cname (in-list (constraint-names const))])
                              (memq cname singleton-var-names))))
     const)))

(define/contract (forward-check prob ref-name)
  (csp? name? . -> . csp?)
  (define checked-vars (map (λ (vr) (forward-check-var prob ref-name vr)) (vars prob)))
  (when-debug (set! nfchecks (+ (length checked-vars) nchecks)))
  ;; conflict-set will be empty if there are no empty domains (as we would hope)
  (define conflict-set (for/list ([cvr (in-list checked-vars)]
                                  #:when (set-empty? (domain cvr)))
                         (history cvr)))
  ;; for conflict-directed backjumping it's essential to forward-check ALL vars
  ;; (even after an empty domain is generated) and combine their conflicts
  ;; so we can discover the *most recent past var* that could be the culprit.
  ;; If we just bail out at the first conflict, we may backjump too far based on its history
  ;; (and thereby miss parts of the search tree)
  (unless (empty? conflict-set)
    (backtrack! conflict-set))
  ;; Discard constraints that have produced singleton domains
  ;; (they have no further use)
  (prune-singleton-constraints (make-csp checked-vars (constraints prob)) ref-name))

(define/contract (constraint-checkable? const names)
  (constraint? (listof name?) . -> . any/c)
  ;; constraint is checkable if all constraint names
  ;; are in target list of names.
  (for/and ([cname (in-list (constraint-names const))])
    (memq cname names)))

(define/contract (constraint-arity const)
  (constraint? . -> . natural?)
  (length (constraint-names const)))

(define/contract (singleton-var? var)
  (var? . -> . boolean?)
  (= 1 (domain-length var)))

(define/contract (check-constraints prob [mandatory-names #f] #:conflicts [conflict-count? #f])
  ((csp?) ((listof name?) #:conflicts boolean?) . ->* . (or/c csp? natural?))
  (define assigned-varnames (map var-name (assigned-vars prob)))
  (define-values (checkable-consts other-consts)
    (partition (λ (const) (and (constraint-checkable? const assigned-varnames)
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
  (define-values (unary-constraints other-constraints)
    (partition one-arity? (constraints prob)))
  (if (empty? unary-constraints)
      prob
      (make-csp
       (for/list ([vr (in-vars prob)])
         (match-define (var name vals) vr)
         (define name-constraints (filter (λ (const) (constraint-relates? const name)) unary-constraints))
         (make-var name (for/set ([val (in-set vals)]
                                  #:when (for/and ([const (in-list name-constraints)])
                                           ((constraint-proc const) val)))
                          val)))
       other-constraints)))

(define ((make-hist-proc assocs) . xs)
  (not
   (for/and ([x (in-list xs)]
             [val (in-list (map cdr assocs))])
     (equal? x val))))

(define/contract (backtracking-solver
                  prob
                  #:select-variable [select-unassigned-variable
                                     (or (current-select-variable) first-unassigned-variable)]
                  #:order-values [order-domain-values (or (current-order-values) first-domain-value)]
                  #:inference [inference (or (current-inference) forward-check)])
  ((csp?) (#:select-variable procedure? #:order-values procedure? #:inference procedure?) . ->* . generator?)
  (generator ()
             (define starting-state-count (state-count prob))
             (define states-examined 0)
             (define reduce-arity-proc (if (current-arity-reduction) reduce-constraint-arity values))
             (let loop ([prob ((if (current-node-consistency) make-nodes-consistent values) prob)])
               (match (select-unassigned-variable prob)
                 [#false (yield prob)]
                 [(var name domain)
                  (define (wants-backtrack? exn)
                    (and (backtrack? exn) (or (let ([bths (backtrack-histories exn)])
                                                (or (empty? bths) (for*/or ([bth (in-list bths)]
                                                                            [rec (in-list bth)])
                                                                    (eq? name (car rec))))))))
                  (for/fold ([conflicts null]
                             #:result (void))
                            ([val (in-list (order-domain-values (set->list domain)))])
                    (with-handlers ([wants-backtrack?
                                     (λ (bt)
                                       (define bths (backtrack-histories bt))
                                       (append conflicts (remq name (remove-duplicates
                                                                     (for*/list ([bth (in-list bths)]
                                                                                 [rec (in-list bth)])
                                                                       (car rec)) eq?))))])
                      (let* ([prob (assign-val prob name val)]
                             ;; reduce constraints before inference,
                             ;; to create more forward-checkable (binary) constraints
                             [prob (reduce-arity-proc prob)]
                             [prob (inference prob name)]
                             [prob (check-constraints prob)])
                        (loop prob))
                      ;; conflicts goes inside the handler expression
                      ;; so raises can supersede it
                      conflicts))]))))

(define/contract (random-pick xs)
  ((non-empty-listof any/c) . -> . any/c)
  (match xs
    [(list x) x]
    [xs (list-ref xs (random (length xs)))]))

(define (assign-random-vals prob)
  (for/fold ([new-csp prob])
            ([name (in-var-names prob)])
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
  (for/list ([name (in-var-names prob)]
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

(define/contract (csp->assocs prob [keys #f])
  ((csp?) ((listof name?)) . ->* . (listof (cons/c name? any/c)))
  (define assocs
    (for/list ([vr (in-vars prob)])
      (match vr
        [(var name (list val)) (cons name val)])))
  (if keys
      (for/list ([key (in-list keys)])
        (assq key assocs))
      assocs))

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
              #:when (constraint-checkable? const names))
     const)))

(define (decompose-prob prob)
  ; decompose into independent csps. `cc` determines "connected components"
  (if (current-decompose)
      (for/list ([nodeset (in-list (cc (csp->graph prob)))])
        (extract-subcsp prob nodeset))
      (list prob)))

(define (make-solution-generator prob)
  (generator ()
             (define subprobs (decompose-prob prob))
             (define solgens (map (current-solver) subprobs))
             (define solstreams (for/list ([solgen (in-list solgens)])
                                  (for/stream ([sol (in-producer solgen (void))]) 
                                    sol)))
             (for ([solution-pieces (in-cartesian solstreams)])
               (yield (combine-csps solution-pieces)))))

(define-syntax-rule (in-solutions PROB)
  (in-producer (make-solution-generator PROB) (void)))

(define/contract (solve* prob [max-solutions +inf.0]
                         #:finish-proc [finish-proc (λ (p) (csp->assocs p (map var-name (vars prob))))]
                         #:solver [solver #f])
  ((csp?) (natural? #:finish-proc procedure? #:solver procedure?) . ->* . (listof any/c))
  (when-debug (reset-nassns!) (reset-nfchecks!) (reset-nchecks!))          

  (parameterize ([current-solver (or solver (current-solver) backtracking-solver)])
    (for/list ([sol (in-solutions prob)]
               [idx (in-range max-solutions)])
      (finish-proc sol))))

(define/contract (solve prob
                        #:finish-proc [finish-proc (λ (p) (csp->assocs p (map var-name (vars prob))))]
                        #:solver [solver #f])
  ((csp?) (#:finish-proc procedure? #:solver procedure?)
          . ->* . (or/c #false any/c))
  (match (solve* prob 1 #:finish-proc finish-proc #:solver solver)
    [(list solution) solution]
    [_ #false]))

(define (<> a b) (not (= a b)))
(define (neq? a b) (not (eq? a b)))

