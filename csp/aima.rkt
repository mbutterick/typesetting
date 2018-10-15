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
  ($csp? variable? any/c . -> . (listof removal?))
  ;; Start accumulating inferences from assuming var=value
  (support_pruning csp)
  (define removals
    (for/list ([a (hash-ref ($csp-curr_domains csp) var)]
               #:when (not (equal? a value)))
      (cons var a)))
  (hash-set! ($csp-curr_domains csp) var (list value))
  removals)

;; todo: update uses of `prune` to be functional on removals
(define/contract (prune csp var value removals)
  ($csp? variable? any/c (or/c #f (listof removal?)) . -> . (listof removal?))
  ;; Rule out var=value
  (hash-update! ($csp-curr_domains csp) var (λ (vals) (remove value vals)))
  (and removals (append removals (list (cons var value)))))

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
  ($csp? (listof removal?) . -> . void?)
  ;; Undo a supposition and all inferences from it.
  (for ([removal (in-list removals)])
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

;; Value ordering

(define/contract (unordered_domain_values var assignment csp)
  (variable? assignment? $csp? . -> . (listof any/c))
  ;; The default value order.
  (choices csp var))

;; Inference

(define/contract (no_inference csp var value assignment removals)
  ($csp? variable? any/c assignment? (listof removal?) . -> . boolean?)
  #true)

(define/contract (backtracking_search csp
                                      [select_unassigned_variable first_unassigned_variable]
                                      [order_domain_values unordered_domain_values]
                                      [inference no_inference])
  (($csp?) (procedure? procedure? procedure?) . ->* . (or/c #f assignment?))
  (define (backtrack [assignment (make-hasheq)])
    ;; todo: convert to generator with `yield`
    (let/ec return
      (when (all-variables-assigned? csp assignment)
        (return assignment))
      (define var (select_unassigned_variable assignment csp))
      (for ([val (in-list (order_domain_values var assignment csp))]
            #:when (zero? (nconflicts csp var val assignment)))
        (assign csp var val assignment)
        (define removals (suppose csp var val))
        (when (inference csp var val assignment removals)
          (define result (backtrack assignment))
          (when result
            (return result))
          (restore csp removals)))
      (unassign csp var assignment)
      (return #false)))

  (define result (backtrack))
  (unless (or (false? result) (goal_test csp result))
    (error 'whut))
  result)

;; todo: make multiple results work
(define/contract (solve* csp [solver backtracking_search] [finish-proc values]
                         #:count [solution-limit +inf.0])
  (($csp?) (procedure? procedure? #:count integer?) . ->* . (or/c #f (non-empty-listof any/c)))
  (match (for/list ([solution (in-value (solver csp))] ; needs generator here
                    [idx (in-range solution-limit)])
           (finish-proc solution))
    [(? pair? solutions) solutions]
    [else #f]))

(define/contract (solve csp [solver backtracking_search] [finish-proc values])
  (($csp?) (procedure? procedure?) . ->* . any/c)
  (match (solve* csp solver finish-proc #:count 1)
    [(list solution) solution]
    [else #f]))

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

(check-equal? (suppose csp 'wa 'red) '((wa . green) (wa . blue)))
(check-equal?
 (hash-ref ($csp-curr_domains csp) 'wa) '(red))

(check-equal? (prune csp 'v 'red empty) '((v . red)))

(check-equal? (choices csp 'v) '(green blue))
(check-equal? (choices csp 'wa) '(red))
(check-equal? (infer_assignment csp)
              (make-hasheq '((wa . red))))
(check-equal? (suppose csp 'v 'blue) '((v . green)))
(check-equal? (infer_assignment csp)
              (make-hasheq '((v . blue) (wa . red))))
(restore csp '((wa . green)))
(check-equal? (infer_assignment csp)
              (make-hasheq '((v . blue))))
(restore csp '((v . blue)))
(check-equal? (infer_assignment csp) (make-hasheq))

(check-equal? (first_unassigned_variable (hash) csp) 'wa)
(check-equal? (unordered_domain_values 'wa (hash) csp) '(red green))

(set-$csp-curr_domains! csp #f) ; reset current domains

(check-equal? (solve csp)
              (make-hasheq '((nsw . green) (nt . green) (q . red) (sa . blue) (t . blue) (v . red) (wa . red))))

(set-$csp-curr_domains! csp #f)
(check-equal? (suppose csp 'nsw 'red) '((nsw . green) (nsw . blue)))
(check-equal? (solve csp)
              (make-hasheq '((nsw . red) (nt . red) (q . green) (sa . blue) (t . blue) (v . green) (wa . green))))
