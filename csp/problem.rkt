#lang racket/base
(require racket/class sugar/container sugar/debug racket/contract racket/match racket/generator racket/list)
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
           [_variable-domains #f])
    
    (reset) ; use method rather than manually set up fields
    
    ;; implement object printing
    (define (repr) (format "<problem% ~a>" (hash-keys _variable-domains)))
    (define/public (custom-print out quoting-depth) (print (repr) out))
    (define/public (custom-display out) (displayln (repr) out))
    (define/public (custom-write out) (write (repr) out))
    
    (define/public (reset)
      ;; Reset the current problem definition
      (set! _constraints null)
      (set! _variable-domains (make-hash)))
    
    (define/public (set-solver solver)
      ;; Set the problem solver currently in use
      (set! _solver solver))
    
    (define/public (get-solver)
      ;; Get the problem solver currently in use
      _solver)
    
    (define/public (add-variable variable domain-or-values)
      ;; Add a variable to the problem
      ;; Contract insures input is Domain object or list of values.
      (when (hash-has-key? _variable-domains variable)
        (error 'add-variable (format "Tried to insert duplicated variable ~a" variable)))      
      (define domain (if (domain%? domain-or-values)
                         (send domain-or-values copy)
                         (new domain% [set domain-or-values])))
      (when (null? (get-field _list domain)) 
        (error 'add-variable "domain value is null"))
      (hash-set! _variable-domains variable domain))
    
    (define/public (add-variables variables domain)
      ;; Add one or more variables to the problem
      (define listified-variables
        (cond 
          [(string? variables) (map (λ(c) (format "~a" c)) (string->list variables))]
          [else variables]))
      (for-each (λ(var) (add-variable var domain)) listified-variables))
    
    (define/public (add-constraint constraint-or-proc [variables null])
      ;; Add a constraint to the problem
      ;; contract guarantees input is procedure or constraint% object
      (define constraint (if (procedure? constraint-or-proc)
                             (new function-constraint% [func constraint-or-proc])
                             constraint-or-proc))
      (py-append! _constraints (list constraint variables)))
    
    (define-syntax-rule (solution-macro solution-proc null-proc)
      (begin
        (define-values (domains constraints vconstraints) (_get-args))
        (if (null? domains)
            (if null-proc (null-proc null) null)
            (send _solver solution-proc domains constraints vconstraints))))
    
    (define/public (get-solution)
      ;; Find and return a solution to the problem
      (solution-macro get-solution #f))
    
    (define/public (get-solutions)
      ;; Find and return all solutions to the problem
      (solution-macro get-solutions #f))
    
    (define/public (get-solution-iter)
      ; Return an iterator to the solutions of the problem
      (solution-macro get-solution-iter yield))
    
    (define/public (_get-args)
      (define variable-domains (hash-copy _variable-domains))
      (define all-variables (hash-keys variable-domains))
      
      ;; set up constraints
      (define constraints
        (for/list ([(constraint variables) (in-parallel (map first _constraints) (map second _constraints))])
          (list constraint (if (null? variables) all-variables variables))))
      
      ;; set up vconstraints
      (define vconstraints 
        (hash-copy ; converts for/hash to mutable hash
         (for/hash ([variable (in-hash-keys variable-domains)])
           (values variable null))))
      
      (for ([(constraint variables) (in-parallel (map first constraints) (map second constraints))])
        (for ([variable (in-list variables)])
          (hash-update! vconstraints variable (λ(val) (cons (list constraint variables) val)))))
      ;;(hash-update! vconstraints variable (λ(val) (append val (list (list constraint variables)))))))
      
      (for ([(constraint variables) (in-parallel (map first constraints) (map second constraints))])
        (send constraint preprocess variables variable-domains constraints vconstraints))
      
      (define result (void))
      (let/ec break
        (for/last ([domain (in-hash-values variable-domains)])
          (send domain reset-state)
          (when (null? (get-field _list domain))
            (set! result (list null null null))
            (break)))
        (set! result (list variable-domains constraints vconstraints)))
      (apply values result))
    
    
    ))