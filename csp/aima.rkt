#lang debug racket
(require racket/generator sugar/debug)

(struct $csp (variables domains neighbors constraints initial curr_domains nassigns) #:transparent #:mutable)
(define assignment? hash?)
(define variable? symbol?)
(define removal? (cons/c variable? any/c))

(define/contract (make-csp variables domains neighbors constraints)
  ((listof variable?) hash? hash? procedure? . -> . $csp?)
  ($csp variables domains neighbors constraints null #f 0))

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

(define/contract (nconflicts csp var val assignment)
  ($csp? variable? any/c assignment? . -> . number?)
  ;; Return the number of conflicts var=val has with other variables."""
  ;; Subclasses may implement this more efficiently
  (for/sum ([v (in-list (hash-ref ($csp-neighbors csp) var))]
            #:when (hash-has-key? assignment v))
    (if (($csp-constraints csp) var val v (hash-ref assignment v)) 0 1)))

(define (display csp assignment)
  (displayln csp))

(define/contract (all-variables-assigned? csp assignment)
  ($csp? assignment? . -> . boolean?)
  (= (length (hash-keys assignment)) (length ($csp-variables csp))))

(define/contract (goal_test csp state)
  ($csp? assignment? . -> . boolean?)
  ;; The goal is to assign all variables, with all constraints satisfied.
  (define assignment state)
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
    (box (for/list ([a (hash-ref ($csp-curr_domains csp) var)]
                    #:when (not (equal? a value)))
           (cons var a)))
    (hash-set! ($csp-curr_domains csp) var (list value))))

;; todo: update uses of `prune` to be functional on removals
(define/contract (prune csp var value removals)
  ($csp? variable? any/c (or/c #f (box/c (listof removal?))) . -> . (box/c (listof removal?)))
  ;; Rule out var=value
  (hash-update! ($csp-curr_domains csp) var (λ (vals) (remove value vals)))
  (and removals
       (set-box! removals (append (unbox removals) (list (cons var value))))
       removals))

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
    (match (hash-ref ($csp-curr_domains csp) v)
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


;; ______________________________________________________________________________
;; CSP Backtracking Search

;; Variable ordering

(define/contract (first_unassigned_variable assignment csp)
  (assignment? $csp? . -> . (or/c #false variable?))
  ;; The default variable order.
  (for/first ([var (in-list ($csp-variables csp))]
              #:unless (hash-has-key? assignment var))
    var))

(define current-shuffle (make-parameter #t))

(define/contract (mrv assignment csp)
  (assignment? $csp? . -> . any/c)
  ;; Minimum-remaining-values heuristic.
  ;; with random tiebreaker.
  (define (num_legal_values var)
    (if ($csp-curr_domains csp)
        (length (hash-ref ($csp-curr_domains csp) var))
        ;; todo: is this the same as python `count`?
        (for/sum ([val (in-list (hash-ref ($csp-domains csp) var))]
                  #:when (zero? (nconflicts csp var val assignment)))
          1)))
  (struct $mrv-rec (var num) #:transparent)
  (define recs (sort
                (for/list ([var (in-list ($csp-variables csp))]
                           #:unless (hash-has-key? assignment var))
                  ($mrv-rec var (num_legal_values var)))
                < #:key $mrv-rec-num))
  (first ((if (current-shuffle) shuffle values) (map $mrv-rec-var (takef recs (λ (rec) (= ($mrv-rec-num (first recs))
                                                                                          ($mrv-rec-num rec))))))))

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
  (for/and ([B (in-list (hash-ref ($csp-neighbors csp) var))]
            #:unless (hash-has-key? assignment B))
    (for ([b (in-list (hash-ref ($csp-curr_domains csp) B))]
          #:unless (($csp-constraints csp) var value B b))
      (prune csp B b removals))
    (not (empty? (hash-ref ($csp-curr_domains csp) B)))))

(define current-select-variable (make-parameter first_unassigned_variable))
(define current-order-values (make-parameter unordered_domain_values))
(define current-inference (make-parameter no_inference))

(define/contract (backtracking_search csp
                                      [select_unassigned_variable (current-select-variable)]
                                      [order_domain_values (current-order-values)]
                                      [inference (current-inference)])
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

(define current-reset (make-parameter #t))

(define/contract (solve* csp [solver backtracking_search] [finish-proc values]
                         #:count [solution-limit +inf.0])
  (($csp?) (procedure? procedure? #:count integer?) . ->* . (or/c #f (non-empty-listof any/c)))
  (begin0
    (match (for/list ([solution (in-producer (solver csp) (void))]
                      [idx (in-range solution-limit)])
             (finish-proc solution))
      [(? pair? solutions) solutions]
      [else #false])
    (when (current-reset)
      (set-$csp-curr_domains! csp #f))))

(define/contract (solve csp [solver backtracking_search] [finish-proc values])
  (($csp?) (procedure? procedure?) . ->* . any/c)
  (match (solve* csp solver finish-proc #:count 1)
    [(list solution) solution]
    [else #false]))

(require rackunit)
(define vs '(wa nsw t q nt v sa))
(define ds (for/hash ([k vs])
             (values k '(red green blue))))
(define ns (for*/hash ([(i ns) (in-dict
                                '((wa nt sa)
                                  (nt wa sa q)
                                  (q nt sa nsw)
                                  (nsw q sa v)
                                  (v sa nsw)
                                  (sa wa nt q nsw v)
                                  (t)))])
             (values i ns)))
(define csp (make-csp vs ds ns (λ (A a B b) (not (equal? a b)))))
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
(check-equal?
 (hash-ref ($csp-curr_domains csp) 'wa) '(red))

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
(check-equal? (length (solve* csp)) 18)

(check-equal? (suppose csp 'nsw 'red) '#&((nsw . green) (nsw . blue)))
(check-equal? (solve csp)
              (make-hasheq '((nsw . red) (nt . red) (q . green) (sa . blue) (t . blue) (v . green) (wa . green))))

(check-equal? (suppose csp 'nsw 'red) '#&((nsw . green) (nsw . blue)))
(check-equal? (length (solve* csp)) 6)

(parameterize ([current-select-variable mrv]
               [current-shuffle #f])
  (check-equal?
   (solve csp)
   (make-hasheq '((nsw . green) (nt . green) (q . red) (sa . blue) (t . blue) (v . red) (wa . red)))))

(parameterize ([current-order-values lcv])
  (check-equal?
   (solve csp)
   (make-hasheq '((nsw . green) (nt . green) (q . red) (sa . blue) (t . blue) (v . red) (wa . red)))))

(parameterize ([current-inference forward_checking])
    (forward_checking csp 'sa 'blue (make-hasheq) (box null))
    (check-equal? ($csp-curr_domains csp)
                  (make-hasheq '((nsw . (red green)) (nt . (red green)) (q . (red green)) (sa . (red green blue)) (t . (red green blue)) (v . (red green)) (wa . (red green))))))

(set-$csp-curr_domains! csp #f)
(parameterize ([current-inference forward_checking]
               [current-reset #f])
  (support_pruning csp)
  (check-equal?
   (solve csp)
   (make-hasheq '((nsw . green) (nt . green) (q . red) (sa . blue) (t . blue) (v . red) (wa . red)))))

