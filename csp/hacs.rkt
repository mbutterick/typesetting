#lang debug racket
(require racket/generator)
(provide (all-defined-out))

(struct $csp ([vars #:mutable]
              [constraints #:mutable]) #:transparent)
(struct $constraint (names proc) #:transparent)

(struct $var (name domain) #:transparent) 
(define $var-name? symbol?)

(struct $cvar $var (past) #:transparent)
(struct $avar $var () #:transparent)

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

(define/contract (assign-val csp name val)
  ($csp? $var-name? any/c . -> . $csp?)
  ($csp
   (for/list ([var ($csp-vars csp)])
     (if (eq? name ($var-name var))
         ($avar name (list val))
         var))
   ($csp-constraints csp)))

(define (unassigned-vars csp)
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
  (struct $mrv-rec (var num) #:transparent)
  (match (unassigned-vars csp)
    [(? empty?) #false]
    [xs (argmin-random-tie (λ (var) (length ($var-domain var))) xs)]))

(define first-domain-value values)

(define (no-inference csp name) csp)

(define/contract (relating constraints names)
  ((listof $constraint?) (listof $var-name?) . -> . (listof $constraint?))
  (for*/list ([constraint (in-list constraints)]
              [cnames (in-value ($constraint-names constraint))]
              #:when (for/and ([name (in-list names)])
                       (memq name cnames)))
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
       (match (($csp-constraints csp) . relating . (list aname name))
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
  ($csp checked-vars ($csp-constraints csp)))

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
                    (and ($backtrack? exn) (memq name ($backtrack-names exn))))
                  (for/fold ([conflicts null]
                             #:result (void))
                            ([val (in-list (order-domain-values domain))])
                    (with-handlers ([wants-backtrack?
                                     (λ (bt) (append conflicts (remq name ($backtrack-names bt))))])
                      (define csp-with-assignment (assign-val csp name val))
                      (loop (inference csp-with-assignment name)))
                    conflicts)]))))

(define/contract (solution-consistent? solution)
  ($csp? . -> . boolean?)
  (for/and ([c (in-list ($csp-constraints solution))])
    (apply ($constraint-proc c) (for*/list ([name (in-list ($constraint-names c))]
                                            [var (in-list ($csp-vars solution))]
                                            #:when (eq? name ($var-name var)))
                                  (first ($var-domain var))))))

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
    (unless (solution-consistent? solution)
      (raise (list 'wtf solution)))
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

