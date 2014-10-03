#lang racket/base
(require racket/class sugar/container "helper.rkt" "variable.rkt")
(provide (all-defined-out))


(define constraint%
  (class object% 
    (super-new)
    
    (define/public (call variables domains assignments [forwardcheck #f])
      ;; Perform the constraint checking
      
      ;;  If the forwardcheck parameter is not false, besides telling if
      ;;  the constraint is currently broken or not, the constraint
      ;;  implementation may choose to hide values from the domains of
      ;;  unassigned variables to prevent them from being used, and thus
      ;;  prune the search space.
      #t)
    
    (define/public (preProcess variables domains constraints vconstraints)
      ;; Preprocess variable domains
      ;;  This method is called before starting to look for solutions,
      ;;  and is used to prune domains with specific constraint logic
      ;;  when possible. For instance, any constraints with a single
      ;;  variable may be applied on all possible values and removed,
      ;;  since they may act on individual values even without further
      ;;  knowledge about other assignments.
      (when (= (length variables) 1)
        (define variable (list-ref variables 0))
        (define domain (hash-ref domains variable))
        (for ([value (in-list (get-field _list domain))])
          
          (when (not (call variables domains (make-hash (list (cons variable value)))))
            (set-field! _list domain (remove value (get-field _list domain)))))
        
        (set! constraints (remove (list this variables) constraints))
        (hash-update! vconstraints variable (λ(val) (remove (list this variables) val)))))
    
    (define/public (forwardCheck variables domains assignments [_unassigned Unassigned])
      ;; Helper method for generic forward checking
      ;;  Currently, this method acts only when there's a single
      ;;  unassigned variable.
      (define return-result #t)
      
      (define unassignedvariable _unassigned)
      ;(report assignments)
      (let/ec break
        (for ([variable (in-list variables)])
          (when (not (variable . in? . assignments))
            (if (equal? unassignedvariable _unassigned)
                (set! unassignedvariable variable)
                (break))))
        (when (not (equal? unassignedvariable _unassigned))
          ;; Remove from the unassigned variable domain's all
          ;; values which break our variable's constraints.
          (define domain (hash-ref domains unassignedvariable))
          ;(report domain domain-fc)
          (when (not (null? (get-field _list domain)))
            (for ([value (in-list (get-field _list domain))])
              (hash-set! assignments unassignedvariable value)
              (when (not (send this call variables domains assignments))
                (send domain hideValue value)))
            (hash-remove! assignments unassignedvariable))
          (when (null? (get-field _list domain))
            (set! return-result #f)
            (break))))
      return-result)
    ))

(define constraint%? (is-a?/c constraint%))

(define function-constraint%
  (class constraint% 
    (super-new)
    (init-field func [assigned #t])
    (field [_func func][_assigned assigned])
    
    (inherit forwardCheck)
    (define/override (call variables domains assignments [forwardcheck #f] [_unassigned Unassigned])
      ;(report assignments assignments-before)
      (define parms (for/list ([x (in-list variables)])
                      (if (hash-has-key? assignments x) (hash-ref assignments x)  _unassigned)))
      ;(report assignments assignments-after)
      (define missing (length (filter (λ(v) (equal? v _unassigned)) parms)))
      (if (> missing 0)
          (begin 
            ;(report missing)
            ;(report _assigned)
            ;(report parms)
            ;(report (apply _func parms))
            ;(report forwardcheck)
            ;(report assignments assignments-to-fc)
            (and (or _assigned (apply _func parms))
                 (or (not forwardcheck) (not (= missing 1))
                     (forwardCheck variables domains assignments))))
          (apply _func parms)))
    
    ))
(define function-constraint%? (is-a?/c function-constraint%))

(define all-different-constraint%
  ;; Constraint enforcing that values of all given variables are different
  
  (class constraint% 
    (super-new)
    
    (define/override (call variables domains assignments [forwardcheck #f] [_unassigned Unassigned])
      (define seen (make-hash))
      (define value #f)
      (define domain #f)
      (define return-value (void))
      (let/ec return-k
        (for ([variable (in-list variables)])
          (set! value (if (hash-has-key? assignments variable) 
                          (hash-ref assignments variable)  
                          _unassigned))
          (when (not (equal? value _unassigned))
            (when (value . in? . seen)
              (set! return-value #f)
              (return-k))
            (hash-set! seen value #t)))
        (when forwardcheck
          (for ([variable (in-list variables)])
            (when (not (variable . in? . assignments))
              (set! domain (hash-ref domains variable))
              (for ([value (in-hash-keys seen)])
                (when (value . in? . (get-field _list (hash-ref domains variable)))
                  (send domain hideValue value)
                  (when (null? (get-field _list (hash-ref domains variable)))
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
    
    (define/override (call variables domains assignments [forwardcheck #f] [_unassigned Unassigned])
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
        (when (and forwardcheck (not (equal? singlevalue _unassigned)))
          (for ([variable (in-list variables)])
            (when (not (variable . in? . assignments))
              (set! domain (hash-ref domains variable))
              (when (not (singlevalue . in? . (get-field _list domain)))
                (set! return-value #f)
                (return-k))
              (for ([value (in-list (get-field _list domain))])
                (when (not (equal? value singlevalue))
                  (send domain hideValue value))))))
        (set! return-value #t)
        (return-k))
      return-value)))

(define all-equal-constraint%? (is-a?/c all-equal-constraint%))
