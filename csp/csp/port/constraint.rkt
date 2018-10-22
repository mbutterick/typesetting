#lang racket/base
(require racket/class racket/bool sugar/unstable/container sugar/list sugar/debug racket/list "helper.rkt" "variable.rkt")
(provide (all-defined-out))

(define constraint%
  (class object% 
    (super-new)
    
    (define/public (is-true? variables domains assignments [forward-check? #f])
      ;; Perform the constraint checking
      
      ;;  If the forwardcheck parameter is not false, besides telling if
      ;;  the constraint is currently broken or not, the constraint
      ;;  implementation may choose to hide values from the domains of
      ;;  unassigned variables to prevent them from being used, and thus
      ;;  prune the search space.
      #t)
    
    (define/public (preprocess variables domains constraints vconstraints)
      ;;  todo: functionalize this
      ;;  Preprocess variable domains
      ;;  This method is called before starting to look for solutions,
      ;;  and is used to prune domains with specific constraint logic
      ;;  when possible. For instance, any constraints with a single
      ;;  variable may be applied on all possible values and removed,
      ;;  since they may act on individual values even without further
      ;;  knowledge about other assignments.
      (when (= (length variables) 1)
        (define variable (car variables))
        (define domain (hash-ref domains variable))
        (set-field! _list domain
                    (for/fold ([domain-values (domain)]) 
                      ([value (in-list (domain))] 
                       #:unless (is-true? variables domains (make-hash (list (cons variable value)))))
                      (remove value domain-values)))        
        (set! constraints (remove (list this variables) constraints))
        (hash-update! vconstraints variable (λ(val) (remove (list this variables) val)))))
    
    ;;  Helper method for generic forward checking
    ;;  Currently, this method acts only when there's a single
    ;;  unassigned variable.
    (define/public (forward-check variables domains assignments [_unassigned Unassigned])
      (define unassigned-variables 
        (filter-not (λ(v) (hash-has-key? assignments v)) variables))
      (cond
        ;; Remove from the unassigned variable's domain 
        ;; all values that break our variable's constraints.
        [(= (length unassigned-variables) 1)
         (define unassigned-variable (car unassigned-variables))
         (define unassigned-variable-domain (hash-ref domains unassigned-variable))
         (for ([value (in-list (unassigned-variable-domain))])
           (hash-set! assignments unassigned-variable value)
           (unless (is-true? variables domains assignments)
             (send unassigned-variable-domain hide-value value)))
         (hash-remove! assignments unassigned-variable)
         (not (empty? unassigned-variable-domain))] ; if domain had no remaining values, the constraint will be impossible to meet, so return #f
        [else #t]))
    ))

(define constraint%? (is-a?/c constraint%))

(define function-constraint%
  (class constraint% 
    (super-new)
    
    (init-field func [assigned #t])
    
    (field [_func func][_assigned assigned])    
    
    (inherit forward-check)
    
    (define/override (is-true? variables domains assignments [forward-check? #f] [_unassigned Unassigned])
      (define parms (map (λ(v) (hash-ref assignments v _unassigned)) variables))
      (define missing (length (filter (λ(p) (equal? p _unassigned)) parms)))
      (if (> missing 0)
          (and (or _assigned (apply _func parms))
               (or (not forward-check?) (not (= missing 1))
                   (forward-check variables domains assignments)))
          (apply _func parms)))))

(define function-constraint%? (is-a?/c function-constraint%))

;; Constraint enforcing that values of all given variables are different
(define all-different-constraint%
  (class constraint% 
    (super-new)
    
    (define/override (is-true? variables domains assignments [forward-check? #f] [_unassigned Unassigned])
      (define-values (assigned-vars unassigned-vars) 
        (partition (λ(var) (hash-has-key? assignments var)) variables))      
      (define assigned-values (map (λ(var) (hash-ref assignments var)) assigned-vars))
      (cond
        [(not (members-unique? assigned-values)) #f] ; constraint failed because they're not all different
        [(and forward-check?
              (for*/or ([unassigned-var-domain (in-list (map (λ(uv) (hash-ref domains uv)) unassigned-vars))]
                        [assigned-value (in-list assigned-values)]
                        #:when (member assigned-value (unassigned-var-domain)))
                (send unassigned-var-domain hide-value assigned-value)
                (empty? unassigned-var-domain))) #f] ; if domain had no remaining values, the constraint will be impossible to meet, so return #f
        [else #t]))))

(define all-different-constraint%? (is-a?/c all-different-constraint%))

;; Constraint enforcing that values of all given variables are different
(define all-equal-constraint%
  (class constraint% 
    (super-new)
    
    (define/override (is-true? variables domains assignments [forward-check? #f] [_unassigned Unassigned])
      (define-values (assigned-vars unassigned-vars) 
        (partition (λ(var) (hash-has-key? assignments var)) variables))      
      (define assigned-values (map (λ(var) (hash-ref assignments var)) assigned-vars))
      (define single-value (if (not (empty? assigned-values)) 
                               (car assigned-values)
                               _unassigned))
      (cond
        [(not (andmap (λ(v) (equal? single-value v)) assigned-values)) #f] ; constraint broken: not all values the same
        [(and forward-check? (not (equal? single-value _unassigned))) 
         (for/and ([unassigned-var-domain (in-list (map (λ(uv) (hash-ref domains uv)) unassigned-vars))])
           ;; if single-value is not a member of each domain, constraint will be broken later, so bail out
           (and (member single-value (unassigned-var-domain))
                (for ([value (in-list (unassigned-var-domain))]
                      #:unless (equal? value single-value))
                  (send unassigned-var-domain hide-value value))))] ; otherwise hide nonconforming values
        [else #t]))))


(define all-equal-constraint%? (is-a?/c all-equal-constraint%))

