#lang racket/base
(require racket/class sugar/container sugar/debug racket/contract racket/match)
(require "domain.rkt" "helper.rkt" "constraint.rkt" "solver.rkt")
(provide (all-defined-out))

(define/contract problem%
  ;; Class used to define a problem and retrieve solutions
  
  (class/c [reset (->m void?)]
           [set-solver (solver%? . ->m . void?)] 
           [get-solver (->m solver%?)]
           ;; todo: tighten `object?` contract
           [add-variable (any/c (or/c list? domain%?) . ->m . void?)]
           [add-variables ((listof any/c) (or/c list? domain%?) . ->m . void?)]
           [add-constraint (((or/c constraint%? procedure?)) ((listof any/c)) . ->*m . void?)]
           [get-solution (->m any/c)]
           [get-solutions (->m list?)]
           [_get-args (->m (values (listof domain%?) (listof constraint%?) (listof hash?)))])
  
  (class* object% (printable<%>)
    (super-new)
    
    (init-field [solver #f])
    (field [_solver (or solver (new backtracking-solver%))]
           [_constraints #f]
           [_variables #f])
    
    (reset)
    
    (define (repr) (format "<problem% ~a>" (hash-keys _variables)))
    (define/public (custom-print out quoting-depth) (print (repr) out))
    (define/public (custom-display out) (displayln (repr) out))
    (define/public (custom-write out) (write (repr) out))
    
    (define/public (reset)
      ;; Reset the current problem definition
      (set! _constraints null)
      (set! _variables (make-hash)))
    
    (define/public (set-solver solver)
      ;; Change the problem solver currently in use
      (set! _solver solver))
    
    (define/public (get-solver)
      ;; Obtain the problem solver currently in use
      _solver)
    
    (define/public (add-variable variable domain-or-values)
      ;; Add a variable to the problem
      ;; Contract insures input is Domain object or list of values.
      (when (hash-has-key? _variables variable)
        (error 'add-variable (format "Tried to insert duplicated variable ~a" variable)))      
      (define domain (if (domain%? domain-or-values)
                         (send domain-or-values copy)
                         (new domain% [set domain-or-values])))
      (when (not (object? domain)) (error 'add-variable "not a Domain object"))
      (when (null? (get-field _list domain)) (error 'add-variable "domain value is null"))
      (hash-set! _variables variable domain))
    
    (define/public (add-variables variables domain)
      ;; Add one or more variables to the problem
      (define listified-variables
        (cond 
          [(string? variables) (map (λ(c) (format "~a" c)) (string->list variables))]
          [else variables]))
      (for-each (λ(var) (add-variable var domain)) listified-variables))
    
    (define/public (add-constraint constraint [variables null])
      ;; Add a constraint to the problem
      
      (when (not (constraint%? constraint))
        (if (procedure? constraint)
            (set! constraint (new function-constraint% [func constraint]))
            (error 'add-constraint "Constraints must be instances of class Constraint")))
      (py-append! _constraints (list constraint variables)))
    
    (define/public (get-solution)
      ;; Find and return a solution to the problem
      (define-values (domains constraints vconstraints) (_get-args))
      (if (not domains)
          null
          (send _solver get-solution domains constraints vconstraints)))
    
    (define/public (get-solutions)
      ;; Find and return all solutions to the problem
      (define-values (domains constraints vconstraints) (_get-args))
      (if (not domains)
          null
          (send _solver get-solutions domains constraints vconstraints)))
    
    (define/public (get-solution-iter)
      ; Return an iterator to the solutions of the problem
      (void))
    
    (define/public (_get-args)
      (define domains (hash-copy _variables))
      (define allvariables (hash-keys domains))
      (define constraints null)
      (for ([constraint-variables-pair (in-list _constraints)])
        (match-define (list constraint variables) constraint-variables-pair)
        (when (null? variables)
          (set! variables allvariables))
        (set! constraints (append constraints (list (list constraint variables)))))
      (define vconstraints (make-hash))
      (for ([variable (in-hash-keys domains)])
        (hash-set! vconstraints variable null))
      (for ([constraint-variables-pair (in-list constraints)])
        (match-define (list constraint variables) constraint-variables-pair)
        (for ([variable (in-list variables)])
          (hash-update! vconstraints variable (λ(val) (append val (list (list constraint variables)))))))
      (for ([constraint-variables-pair (in-list constraints)])
        (match-define (list constraint variables) constraint-variables-pair)
        (send constraint preProcess variables domains constraints vconstraints))
      (define result #f)
      (let/ec done
        (for ([domain (in-list (hash-values domains))])
          (send domain resetState)
          (when (not domain)
            (set! result (list null null null))
            (done)))
        (set! result (list domains constraints vconstraints)))
      (apply values result))
    
    
    ))