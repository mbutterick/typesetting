#lang racket/base
(require racket/class sugar/container racket/contract racket/match "domain.rkt" "helper.rkt" "constraint.rkt" "solver.rkt")
(provide (all-defined-out))

(define/contract Problem
  ;; Class used to define a problem and retrieve solutions
  
  (class/c [reset (->m void?)]
           ;; todo: tighten `object?` contracts
           [setSolver (Solver? . ->m . void?)] 
           [getSolver (->m Solver?)]
           ;; todo: tighten `object?` contract
           [addVariable (any/c (or/c list? object?) . ->m . void?)]
           [getSolutions (->m list?)])
  (class* object% (printable<%>)
    (super-new)
    
    (init-field [solver #f])
    (field [_solver (or solver (new BacktrackingSolver))]
           [_constraints null]
           [_variables (make-hash)])
    
    
    (define (repr) (format "<Problem ~a>" (hash-keys _variables)))
    (define/public (custom-print out quoting-depth) (print (repr) out))
    (define/public (custom-display out) (displayln (repr) out))
    (define/public (custom-write out) (write (repr) out))
    
    (define/public (reset)
      ;; Reset the current problem definition
      (set! _constraints null)
      (hash-clear! _variables))
    
    (define/public (setSolver solver)
      ;; Change the problem solver currently in use
      (set! _solver solver))
    
    (define/public (getSolver)
      ;; Obtain the problem solver currently in use
      _solver)
    
    (define/public (addVariable variable domain)
      ;; Add a variable to the problem
      (when (variable . in? . _variables)
        (error 'addVariable (format "Tried to insert duplicated variable ~a" variable)))
      (cond 
        [(list? domain) (set! domain (new Domain [set domain]))]
        ;; todo: test for `instance-of-Domain?` ; how to copy domain?
        [(object? domain) (set! domain '(copy.copy domain))]
        [else (error 'addVariable "Domains must be instances of subclasses of Domain")])
      (when (not (object? domain)) (error 'fudge))
      (when (not domain) ; todo: check this test
        (error 'addVariable "Domain is empty"))
      (hash-set! _variables variable domain))
    
    (define/public (addVariables variables domain)
      ;; Add one or more variables to the problem
      (define listified-variables
        (cond 
          [(string? variables) (map (λ(c) (format "~a" c)) (string->list variables))]
          [else variables]))
      (for-each (λ(var) (addVariable var domain)) listified-variables))
    
    (define/public (addConstraint constraint [variables null])
      ;; Add a constraint to the problem
      
      (when (not (Constraint? constraint))
        (if (procedure? constraint)
            (set! constraint (new FunctionConstraint [func constraint]))
            (error 'addConstraint "Constraints must be instances of class Constraint")))
      (py-append! _constraints (list constraint variables)))
    
    (define/public (getSolution)
      ;; Find and return a solution to the problem
      (define-values (domains constraints vconstraints) (_getArgs))
      (if (not domains)
          null
          (send _solver getSolution domains constraints vconstraints)))
    
    (define/public (getSolutions)
      ;; Find and return all solutions to the problem
      (define-values (domains constraints vconstraints) (_getArgs))
      (if (not domains)
          null
          (send _solver getSolutions domains constraints vconstraints)))
    
    (define/public (_getArgs)
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