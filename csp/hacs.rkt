#lang debug racket
(require racket/generator)
(provide (all-defined-out))

(define-syntax-rule (in-cartesian x)
  (in-generator (let ([argss x])
                  (let loop ([argss argss][acc empty])
                    (if (null? argss)
                        (yield (reverse acc))
                        (for ([arg (in-list (car argss))])
                             (loop (cdr argss) (cons arg acc))))))))

(struct $csp ([vars #:mutable]
              [constraints #:mutable]) #:transparent)
(struct $constraint (names proc) #:transparent
  #:property prop:procedure
  (λ (constraint csp)
    (unless ($csp? csp)
      (raise-argument-error '$constraint-proc "$csp" csp))
    ;; apply proc in many-to-many style
    (for/and ([args (in-cartesian (map (λ (cname) ($csp-vals csp cname)) ($constraint-names constraint)))])
             (apply ($constraint-proc constraint) args))))

(struct $var (name domain) #:transparent) 
(define $var-name? symbol?)
(define $var-vals $var-domain)

(struct $cvar $var (past) #:transparent)
(struct $avar $var () #:transparent)


(define (make-csp [vds null] [constraints null])
  ($csp vds constraints))

(define/contract (add-vars! csp names-or-procedure [vals-or-procedure empty])
  (($csp? (or/c (listof $var-name?) procedure?)) ((or/c (listof any/c) procedure?)) . ->* . void?)
  (for/fold ([vars ($csp-vars csp)]
             #:result (set-$csp-vars! csp vars))
            ([name (in-list (if (procedure? names-or-procedure)
                                (names-or-procedure)
                                names-or-procedure))])
    (when (memq name (map $var-name vars))
      (raise-argument-error 'add-vars! "var that doesn't already exist" name))
    (append vars (list ($var name
                             (if (procedure? vals-or-procedure)
                                 (vals-or-procedure)
                                 vals-or-procedure))))))

(define/contract (add-var! csp name [vals-or-procedure empty])
  (($csp? $var-name?) ((or/c (listof any/c) procedure?)) . ->* . void?)
  (add-vars! csp (list name) vals-or-procedure))

(define/contract (add-constraints! csp proc namess [proc-name #false])
  (($csp? procedure? (listof (listof $var-name?))) ((or/c #false $var-name?)) . ->* . void?)
  (set-$csp-constraints! csp (append ($csp-constraints csp) 
                                     (for/list ([names (in-list namess)])
                                               (for ([name (in-list names)])
                                                    (check-name-in-csp! 'add-constraints! csp name))
                                               ($constraint names (if proc-name
                                                                      (procedure-rename proc proc-name)
                                                                      proc))))))

(define/contract (add-pairwise-constraint! csp proc var-names [proc-name #false])
  (($csp? procedure? (listof $var-name?)) ($var-name?) . ->* . void?)
  (add-constraints! csp proc (combinations var-names 2) proc-name))

(define/contract (add-constraint! csp proc var-names [proc-name #false])
  (($csp? procedure? (listof $var-name?)) ($var-name?) . ->* . void?)
  (add-constraints! csp proc (list var-names) proc-name))

(define/contract (alldiff= x y)
  (any/c any/c . -> . boolean?)
  (not (= x y)))

(struct inconsistency-signal (csp) #:transparent)

(struct $backtrack (names) #:transparent)

(define current-select-variable (make-parameter #f))
(define current-order-values (make-parameter #f))
(define current-inference (make-parameter #f))
(define current-solver (make-parameter #f))
(define current-shuffle (make-parameter #t))

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
  ($var-domain ($csp-var csp name)))

(define order-domain-values values)

(define/contract (assigned-name? csp name)
  ($csp? $var-name? . -> . boolean?)
  (and (memq name (map $var-name (filter $avar? ($csp-vars csp)))) #true))

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

(define/contract (reduce-constraint-arity csp [minimum-arity 3])
  (($csp?) ((or/c #false exact-nonnegative-integer?)) . ->* . $csp?)
  (let ([assigned-name? (curry assigned-name? csp)])
    (define (partially-assigned? constraint)
      (ormap assigned-name? ($constraint-names constraint)))
    ($csp ($csp-vars csp)
          (for/list ([constraint (in-list ($csp-constraints csp))])
                    (cond
                      [(and (if minimum-arity (<= minimum-arity (constraint-arity constraint)) #true)
                            (partially-assigned? constraint))
                       (match-define ($constraint cnames proc) constraint)
                       ($constraint (filter-not assigned-name? cnames)
                                    ;; pattern is mix of values and symbols (indicating variables to persist)
                                    (let ([reduce-arity-pattern (for/list ([cname (in-list cnames)])
                                                                          (if (assigned-name? cname)
                                                                              (first ($csp-vals csp cname))
                                                                              cname))])
                                      (reduce-arity proc reduce-arity-pattern)))]
                      [else constraint])))))

(define/contract (assign-val csp name val)
  ($csp? $var-name? any/c . -> . $csp?)
  (define assigned-csp ($csp
                        (for/list ([var ($csp-vars csp)])
                                  (if (eq? name ($var-name var))
                                      ($avar name (list val))
                                      var))
                        ($csp-constraints csp)))
  (reduce-constraint-arity assigned-csp))

(define/contract (unassigned-vars csp)
  ($csp? . -> . (listof (and/c $var? (not/c $avar?))))
  (for/list ([var (in-list ($csp-vars csp))]
             #:unless ($avar? var))
            var))

(define/contract (first-unassigned-variable csp)
  ($csp? . -> . (or/c #false (and/c $var? (not/c $avar?))))
  (match (unassigned-vars csp)
    [(? empty?) #false]
    [xs (first xs)]))

(define/contract (argmin-random-tie proc xs)
  (procedure? (non-empty-listof any/c) . -> . any/c)
  (define ordered-xs (sort xs < #:key proc))
  (first ((if (current-shuffle) shuffle values)
          (takef ordered-xs (λ (x) (= (proc (car ordered-xs)) (proc x)))))))

(define/contract (minimum-remaining-values csp)
  ($csp? . -> . (or/c #false (and/c $var? (not/c $avar?))))
  (match (unassigned-vars csp)
    [(? empty?) #false]
    [xs (argmin-random-tie (λ (var) (length ($var-domain var))) xs)]))

(define mrv minimum-remaining-values)

(define/contract (var-degree csp var)
  ($csp? $var? . -> . exact-nonnegative-integer?)
  (for/sum ([constraint (in-list ($csp-constraints csp))]
            #:when (memq ($var-name var) ($constraint-names constraint)))
           1))

(define/contract (blended-variable-selector csp)
  ($csp? . -> . (or/c #false (and/c $var? (not/c $avar?))))
  (define uvars (unassigned-vars csp))
  (cond
    [(empty? uvars) #false]
    [(findf singleton-var? uvars)]
    [else (first (let* ([uvars-by-mrv (sort uvars < #:key (λ (var) (length ($var-domain var))))]
                        [uvars-by-degree (sort uvars-by-mrv > #:key (λ (var) (var-degree csp var)))])
                   uvars-by-degree))]))
  
(define first-domain-value values)

(define (no-inference csp name) csp)

(define/contract (relating-only constraints names)
  ((listof $constraint?) (listof $var-name?) . -> . (listof $constraint?))
  (for*/list ([constraint (in-list constraints)]
              [cnames (in-value ($constraint-names constraint))]
              #:when (and (= (length names) (length cnames))
                          (for/and ([name (in-list names)])
                                   (memq name cnames))))
             constraint))

(define/contract (forward-check csp aname)
  ($csp? $var-name? . -> . $csp?)
  (define aval (first ($csp-vals csp aname)))
  (define (check-var var)
    (match var
      ;; don't check against assigned vars, or the reference var
      ;; (which is probably assigned but maybe not)
      [(? (λ (x) (or ($avar? x) (eq? ($var-name x) aname)))) var]
      [($var name vals)
       (match (($csp-constraints csp) . relating-only . (list aname name))
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
  (define checked-vars (map check-var ($csp-vars csp)))
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
    (raise ($backtrack conflict-set)))
  ;; Discard constraints that have produced singleton domains
  ;; (they have no further use)
  (define nonsingleton-constraints
    (for/list ([constraint (in-list ($csp-constraints csp))]
                #:unless (and
                          (= 2 (constraint-arity constraint)) ; binary constraint
                          (memq aname ($constraint-names constraint)) ; includes target name
                          (let ([other-name (first (remq aname ($constraint-names constraint)))]) ; and something else
                          (= (length ($csp-vals csp other-name)) 1)))) ; that has only one value
               constraint))
  ($csp checked-vars nonsingleton-constraints))

(define/contract (constraint-checkable? c names)
  ($constraint? (listof $var-name?) . -> . boolean?)
  (and (for/and ([cname (in-list ($constraint-names c))])
                (memq cname names))
       #true))

(define/contract (constraint-arity constraint)
  ($constraint? . -> . exact-nonnegative-integer?)
  (length ($constraint-names constraint)))

(define (singleton-var? var)
  (= 1 (length ($var-domain var))))

(define/contract (check-constraints csp)
  ($csp? . -> . $csp?)
  ;; this time, we're not limited to assigned variables
  ;; (that is, vars that have been deliberately assigned in the backtrack process thus far)
  ;; we also want to use "singleton" vars (that is, vars that have been reduced to a single domain value by forward checking)
  (define singleton-varnames (for/list ([var (in-list ($csp-vars csp))]
                                        #:when (singleton-var? var))
                                       ($var-name var)))
  (define-values (checkable-constraints other-constraints)
    (partition (λ (c) (constraint-checkable? c singleton-varnames)) ($csp-constraints csp)))
  (for ([constraint (in-list (sort checkable-constraints < #:key constraint-arity))]
        #:unless (constraint csp))
       (raise ($backtrack null)))
  ($csp ($csp-vars csp) other-constraints))

(define/contract (backtracking-solver
                  csp
                  #:select-variable [select-unassigned-variable
                                     (or (current-select-variable) first-unassigned-variable)]
                  #:order-values [order-domain-values (or (current-order-values) first-domain-value)]
                  #:inference [inference (or (current-inference) no-inference)])
  (($csp?) (#:select-variable procedure? #:order-values procedure? #:inference procedure?) . ->* . generator?)
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
                             [csp (inference csp name)]
                             [csp (check-constraints csp)])
                        (loop csp)))
                    conflicts)]))))



(define/contract ($csp-assocs csp)
  ($csp? . -> . (listof (cons/c $var-name? any/c)))
  (for/list ([var (in-list ($csp-vars csp))])
            (match var
              [($var name domain) (cons name (first domain))])))

(define/contract (solve* csp
                         #:finish-proc [finish-proc $csp-assocs]
                         #:solver [solver (or (current-solver) backtracking-solver)]
                         #:count [max-solutions +inf.0])
  (($csp?) (#:finish-proc procedure? #:solver procedure? #:count integer?) . ->* . (listof any/c))
  (for/list ([solution (in-producer (solver csp) (void))]
             [idx (in-range max-solutions)])
            (finish-proc solution)))

(define/contract (solve csp
                        #:finish-proc [finish-proc $csp-assocs]
                        #:solver [solver (or (current-solver) backtracking-solver)])
  (($csp?) (#:finish-proc procedure? #:solver procedure?) . ->* . (or/c #false any/c))
  (match (solve* csp #:finish-proc finish-proc #:solver solver #:count 1)
    [(list solution) solution]
    [else #false]))

(define (<> a b) (not (= a b)))
(define (neq? a b) (not (eq? a b)))

