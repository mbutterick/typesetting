#lang debug racket
(require racket/generator racket/control sugar/debug)
(provide (all-defined-out))

(struct $csp ([vars #:mutable]
              [constraints #:mutable]) #:transparent)

(define (make-csp [vars null] [constraints null]) ($csp (for/list ([var (in-list vars)])
                                                          (let loop ([var var])
                                                            (match var
                                                              [(list (? symbol? name) vals) (loop ($var name vals))]
                                                              [($var name vals) ($varc name vals null)])))
                                                        constraints))
(define debug (make-parameter #false))

(define-syntax-rule (in-cartesian x)
  (in-generator (let ([argss x])
                  (let loop ([argss argss][acc empty])
                    (if (null? argss)
                        (yield (reverse acc))
                        (for ([arg (in-list (car argss))])
                          (loop (cdr argss) (cons arg acc))))))))

(struct $var (name vals) #:transparent)
(struct $varc $var (culprits) #:transparent)
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

(define/contract (add-vars! csp names-or-procedure [vals-or-procedure empty])
  (($csp? (or/c (listof $var-name?) procedure?)) ((or/c (listof any/c) procedure?)) . ->* . void?)
  (for/fold ([vars ($csp-vars csp)]
             #:result (set-$csp-vars! csp vars))
            ([name (in-list (if (procedure? names-or-procedure)
                                (names-or-procedure)
                                names-or-procedure))])
    (when (memq name (map $var-name vars))
      (raise-argument-error 'add-vars! "var that doesn't already exist" name))
    (append vars (list ($varc name
                              (if (procedure? vals-or-procedure)
                                  (vals-or-procedure)
                                  vals-or-procedure)
                              null)))))

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

(define/contract (no-solutions? csp)
  ($csp? . -> . boolean?)
  (zero? (state-count csp)))

(struct inconsistency-signal (csp) #:transparent)

(define use-reduce-arity? (make-parameter #t))
(define use-mac? (make-parameter #t))
(define use-remove-constraints? (make-parameter #t))
(define use-validate-assignments? (make-parameter #t))

(define/contract (apply-unary-constraint csp constraint #:culprit [culprit #f])
  (($csp? unary-constraint?) (#:culprit (or/c #f $var-name?)) . ->* . $csp?)
  (define (update-csp-vars name vals)
    (for/list ([var (in-list ($csp-vars csp))])
      (define new-culprits (if (and culprit (< (length vals) (length ($var-vals var))))
                               (remove-duplicates (cons culprit ($varc-culprits var)) eq?)
                               ($varc-culprits var)))
      (if (eq? ($var-name var) name)
          ($varc name vals new-culprits)
          var)))    
  (match-define ($constraint (list name) proc) constraint)
  (match (if (promise? proc)
             (force proc)
             (filter proc ($csp-vals csp name)))
    [(list) (raise (inconsistency-signal csp))]
    [(list assigned-val) ((if (use-validate-assignments?) make-nodes-consistent values)
                          ((if (use-remove-constraints?) remove-assigned-constraints values)
                           ((if (use-reduce-arity?) reduce-constraint-arity values)
                            ((if (use-validate-assignments?) validate-assignments values)
                             (let ([csp ($csp
                                         (update-csp-vars name (list assigned-val))
                                         ($csp-constraints csp))])
                               (if (use-mac?)
                                   (make-arcs-consistent csp #:mac name)
                                   csp))))))]
    [(list new-vals ...) ($csp (update-csp-vars name new-vals)
                               ;; once the constraint is applied, it can go away
                               ;; ps this is not the same as an "assigned" constraint
                               ;; because the var may still have multiple values
                               (if (use-remove-constraints?)
                                   (remove constraint ($csp-constraints csp))
                                   ($csp-constraints csp)))]))

(define/contract (make-nodes-consistent csp)
  ($csp? . -> . $csp?)
  (for/fold ([csp csp])
            ([constraint (in-list ($csp-constraints csp))]
             #:when (unary-constraint? constraint))
    (apply-unary-constraint csp constraint)))

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
                                            (string->symbol (format "~a-arc-to-~a" (object-name proc)  other-name))))
                          #:culprit other-name))

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

(define/contract (make-arcs-consistent csp #:mac [mac-name #f])
  (($csp?) (#:mac (or/c $var-name? #f)) . ->* . $csp?)
  ;; csp is arc-consistent if every pair of variables (x y)
  ;; has values in their domain that satisfy every binary constraint
  ;; AC-3 as described by AIMA @ 265
  (define (mac-condition? arc)
    (and
     (constraint-contains-name? ($arc-constraint arc) mac-name)
     (memq ($arc-name arc) (map $var-name (unassigned-vars csp))))) 
  (define starting-arcs
    (for/list ([arc (in-list (binary-constraints->arcs (filter binary-constraint? ($csp-constraints csp))))]
               #:when ((if mac-name mac-condition? values) arc))
      arc))
  (for/fold ([csp csp]
             [arcs starting-arcs]
             #:result csp)
            ([i (in-naturals)]
             #:break (empty? arcs))
    (match-define (cons ($arc name proc) other-arcs) arcs)
    (define reduced-csp (reduce-domains-by-arc csp ($arc name proc)))
    (values reduced-csp (if (= (length ($csp-vals csp name)) (length ($csp-vals reduced-csp name)))
                            ;; revision did not reduce the domain, so keep going
                            other-arcs
                            ;; revision reduced the domain, so supplement the list of arcs
                            (remove-duplicates (append (starting-arcs . terminating-at . name) other-arcs))))))

(define/contract (var-assigned? var)
  ($var? . -> . boolean?)
  (= 1 (remaining-values var)))

(define/contract (solution-complete? csp)
  ($csp? . -> . 'lean?)
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

(define use-mrv? (make-parameter #t))
(define/contract (select-unassigned-var csp)
  ($csp? . -> . (or/c #f $var?))
  (match (unassigned-vars csp)
    [(list) #f]
    [(list uvars ...)
     (cond
       [(use-mrv?)
        ;; minimum remaining values (MRV) rule
        (define mrv-arg (argmin remaining-values uvars))
        (match (filter (λ (var) (= (remaining-values mrv-arg) (remaining-values var))) uvars)
          [(list winning-uvar) winning-uvar] 
          [(list mrv-uvars ...)
           ;; use degree as tiebreaker for mrv
           (define max-degree-arg (argmax (λ (var) (var-degree csp var)) mrv-uvars))
           ;; use random tiebreaker for degree
           (first (shuffle (filter (λ (var) (= (var-degree csp max-degree-arg) (var-degree csp var))) mrv-uvars)))])]
       [else (first uvars)])]))

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
    (raise (inconsistency-signal csp)))
  csp)

(define/contract (assign-val csp name val)
  ($csp? $var-name? any/c . -> . $csp?)
  (check-name-in-csp! 'assign-val csp name)
  (define assignment-constraint ($constraint (list name) (delay (list val))))
  (apply-unary-constraint csp assignment-constraint))

(define/contract (assign-val! csp name val)
  ($csp? $var-name? any/c . -> . void?)
  (check-name-in-csp! 'assign-val! csp name)
  (define new-csp (assign-val csp name val))
  (set-$csp-vars! csp ($csp-vars new-csp))
  (set-$csp-constraints! csp ($csp-constraints new-csp)))

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

(define/contract (assigned-name? csp name)
  ($csp? $var-name? . -> . boolean?)
  (and (memq name (map $var-name (assigned-vars csp))) #true))

(define/contract (reduce-constraint-arity csp [minimum-arity #false])
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
                                                              ($csp-ref csp cname)
                                                              cname))])
                              (reduce-arity proc reduce-arity-pattern)))]
              [else constraint])))))

(define/contract (conflict-set csp name)
  ($csp? $var-name? . -> .  (listof $var-name?))
  ;; earlier assigned variables that participate in constraints with name
  (define assigned-names (reverse (map $var-name (assigned-vars csp))))
  (define earlier-assigned-names (memq name assigned-names))
  (for*/list ([constraint (in-list ($csp-constraints csp))]
              [cnames (in-value ($constraint-names constraint))]
              #:when (and (andmap (λ (cname) (memq cname earlier-assigned-names)) cnames)
                          (constraint-contains-name? constraint name))
              [cname (in-list cnames)]
              #:unless (eq? cname name))
    cname))

(define use-cdj? (make-parameter #f))
(define/contract (select-k sig name krecs)
  (inconsistency-signal? $var-name? (listof (cons/c $var-name? continuation?)) . -> . continuation?)
  ;; select the most recent (ie topmost) k that is in the signal
  ;; todo: repair backjumping
  (cond
   [(use-cdj?)
    (define assigned-names (map car krecs)) ; already in reverse chron order
    (define csp (inconsistency-signal-csp sig))
    (define backjump-dest
      (let loop ([name name][cset (conflict-set csp name)])
        (define next-name (for/first ([previously-assigned-name (in-list (memq name assigned-names))]
                                      #:when (memq previously-assigned-name cset))
                            previously-assigned-name))
        (define next-cset (conflict-set csp next-name))
        (if (empty? next-cset)
            next-name
            (loop next-name (remq next-name (remove-duplicates (append next-cset cset) eq?))))))
    (cdr (assq backjump-dest krecs))]
   [else (cdr (first krecs))]))

(define/contract (backtrack-solution-generator csp)
  ($csp? . -> . generator?)
  ;; as described by AIMA @ 271
  (generator () (let backtrack ([csp (make-arcs-consistent (make-nodes-consistent csp))]
                                [backjump-krecs null])
                  (match (select-unassigned-var csp)
                    [#f (yield ($csp (for/list ([v (in-list ($csp-vars csp))])
                                       (match v
                                         [($varc name vals _) ($var name vals)]
                                         [(? $var? v) v]))
                                     ($csp-constraints csp)))]
                    [($var name vals)
                     (call/prompt
                      (thunk
                       (for ([val (in-list (order-domain-values vals))])
                         (let/cc backjump-k
                           (let ([backjump-krecs (cons (cons name backjump-k) backjump-krecs)])
                             (with-handlers ([inconsistency-signal?
                                              (λ (sig)
                                                (define backjump-k (select-k sig name backjump-krecs))
                                                (backjump-k))])
                               (backtrack (assign-val csp name val) backjump-krecs)))))))]))))

(define/contract (solve* csp [finish-proc values][solution-limit +inf.0])
  (($csp?) (procedure? integer?) . ->* . (non-empty-listof any/c))
  (define solutions
    (for/list ([solution (in-producer (backtrack-solution-generator csp) (void))]
               [idx (in-range solution-limit)])
      (finish-proc solution)))
  (unless (pair? solutions) (raise (inconsistency-signal csp)))
  solutions)

(define/contract (solve csp [finish-proc values])
  (($csp?) (procedure?) . ->* . any/c)
  (first (solve* csp finish-proc 1)))

(define ($csp-ref csp name) (first ($csp-vals csp name)))

(define/contract (alldiff . xs)
  (() #:rest (listof any/c) . ->* . boolean?)
  (= (length (remove-duplicates xs)) (length xs)))

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