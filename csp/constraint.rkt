#lang racket/base
(require racket/class sugar/container sugar/debug racket/list "helper.rkt" "variable.rkt")
(provide (all-defined-out))

(define constraint%
  (class object% 
    (super-new)
    
    (define/public (call variables domains assignments [forward-check? #f])
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
                    (for/fold ([domain-values (send domain get-values)]) 
                      ([value (in-list (send domain get-values))] 
                       #:when (not (call variables domains (make-hash (list (cons variable value))))))
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
         (define domain (hash-ref domains unassigned-variable))
         (for ([value (in-list (send domain get-values))])
           (hash-set! assignments unassigned-variable value)
           (when (not (call variables domains assignments))
             (send domain hide-value value)))
         (hash-remove! assignments unassigned-variable)
         (not (send domain values-empty?))]
        [else #t]))
    ))

(define constraint%? (is-a?/c constraint%))

(define function-constraint%
  (class constraint% 
    (super-new)
    (init-field func [assigned #t])
    (field [_func func][_assigned assigned])
    
    (inherit forward-check)
    
    (define/override (call variables domains assignments [forward-check? #f] [_unassigned Unassigned])
      (define parms (map (λ(v) (hash-ref assignments v _unassigned)) variables))
      (define missing (length (filter (λ(p) (equal? p _unassigned)) parms)))
      (if (> missing 0)
          (and (or _assigned (apply _func parms))
               (or (not forward-check?) (not (= missing 1))
                   (forward-check variables domains assignments)))
          (apply _func parms)))
    
    ))
(define function-constraint%? (is-a?/c function-constraint%))

(define all-different-constraint%
  ;; Constraint enforcing that values of all given variables are different
  
  (class constraint% 
    (super-new)
    
    (define/override (call variables domains assignments [forward-check? #f] [_unassigned Unassigned])
      (define seen (make-hash))
      (define return-value (void))
      
      (let/ec return-k
        (define values (map (λ(v) (hash-ref assignments v _unassigned)) variables))
        (for ([value (in-list values)]
              #:when (not (equal? value _unassigned)))
          (when (hash-has-key? (report seen) value)
            (set! return-value #f)
            (return-k))
          (hash-set! seen value #t))
        
        (when forward-check?
          (for ([variable (in-list variables)])
            (when (not (hash-has-key? assignments variable))
              (let ([domain (hash-ref domains variable)])
                (for ([value (in-hash-keys seen)]
                      #:when (member value (send domain get-values)))
                  (send domain hide-value value)
                  (when (send domain values-empty?)
                    (set! return-value #f)
                    (return-k)))))))
        (set! return-value #t)
        (return-k))
      return-value)))

(define all-different-constraint%? (is-a?/c all-different-constraint%))


(define all-equal-constraint%
  ;; Constraint enforcing that values of all given variables are different
  
  (class constraint% 
    (super-new)
    
    (define/override (call variables domains assignments [forward-check? #f] [_unassigned Unassigned])
      (define singlevalue _unassigned)
      (define value #f)
      (define domain #f)
      (define return-value (void))
      (let/ec return-k
        (for ([variable (in-list variables)])
          (set! value (if (hash-has-key? assignments variable) 
                          (hash-ref assignments variable)  
                          _unassigned))
          (cond
            [(equal? singlevalue _unassigned) (set! singlevalue value)]
            [(and (not (equal? value _unassigned)) (not (equal? value singlevalue)))
             (set! return-value #f)
             (return-k)]))
        (when (and forward-check? (not (equal? singlevalue _unassigned)))
          (for ([variable (in-list variables)])
            (when (not (variable . in? . assignments))
              (set! domain (hash-ref domains variable))
              (when (not (singlevalue . in? . (send domain get-values)))
                (set! return-value #f)
                (return-k))
              (for ([value (in-list (send domain get-values))])
                (when (not (equal? value singlevalue))
                  (send domain hide-value value))))))
        (set! return-value #t)
        (return-k))
      return-value)))

(define all-equal-constraint%? (is-a?/c all-equal-constraint%))
