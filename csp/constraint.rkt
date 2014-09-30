#lang racket/base
(require racket/class racket/contract racket/match)
(require sugar/container sugar/debug)

(module+ test (require rackunit))

;; Adapted from work by Gustavo Niemeyer
#|
# Copyright (c) 2005-2014 - Gustavo Niemeyer <gustavo@niemeyer.net>
# 
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met: 
# 
# 1. Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer. 
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution. 
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(provide (all-defined-out))
;(provide Problem Variable Domain Unassigned Solver BacktrackingSolver RecursiveBacktrackingSolver MinConflictsSolver Constraint FunctionConstraint AllDifferentConstraint AllEqualConstraint MaxSumConstraint ExactSumConstraint MinSumConstraint InSetConstraint NotInSetConstraint SomeInSetConstraint SomeNotInSetConstraint)

;(define Problem/c (λ(x) (is-a x Problem)))

(define/contract Problem
  ;; Class used to define a problem and retrieve solutions
  
  (class/c [reset (->m void?)]
           ;; todo: tighten `object?` contracts
           [setSolver (object? . ->m . void?)] 
           [getSolver (->m object?)]
           ;; todo: tighten `object?` contract
           [addVariable (any/c (or/c list? object?) . ->m . void?)]
           [getSolutions (->m list?)])
  (class object%
    (super-new)
    
    (init-field [solver #f])
    (field [_solver (or solver (new BacktrackingSolver))]
           [_constraints null]
           [_variables (make-hash)])
    
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
      (for-each (λ(var) (addVariable var domain)) variables))
    
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
        (match-define (cons constraint variables) constraint-variables-pair)
        (when (not variables)
          (set! variables allvariables))
        (set! constraints (append constraints (list (cons constraint variables)))))
      (define vconstraints (make-hash))
      (for ([variable (in-hash-keys domains)])
        (hash-set! vconstraints variable null))
      (for ([constraint-variables-pair (in-list constraints)])
        (match-define (cons constraint variables) constraint-variables-pair)
        (for ([variable (in-list variables)])
          (hash-update! vconstraints variable (λ(val) (append val (list (cons constraint variables)))))))
      (for ([constraint-variables-pair (in-list constraints)])
        (match-define (cons constraint variables) constraint-variables-pair)
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

(module+ test
  (check-equal? (get-field _solver (new Problem [solver 'solver-in])) 'solver-in)
  (check-equal? (get-field _constraints (new Problem)) null)
  (check-equal? (get-field _variables (new Problem)) (make-hash))
  
  (define problem (new Problem))
  (send problem addVariable "a" '(1 2))
  (check-equal? (get-field _list (hash-ref (get-field _variables problem) "a")) '(1 2)) 
  (send problem reset)
  (check-equal? (get-field _variables problem) (make-hash))
  (send problem addVariables '("a" "b") '(1 2 3))
  (check-equal? (get-field _list (hash-ref (get-field _variables problem) "a")) '(1 2 3))
  (check-equal? (get-field _list (hash-ref (get-field _variables problem) "b")) '(1 2 3))
  (get-field _variables problem)
  (send problem getSolutions)
  )


;; ----------------------------------------------------------------------
;; Domains
;; ----------------------------------------------------------------------


(define Domain
  ;; Class used to control possible values for variables
  ;; When list or tuples are used as domains, they are automatically
  ;; converted to an instance of that class.
  
  (class object%
    (super-new)
    (init-field set)
    (field [_list set][_hidden null][_states null])
    
    (define/public (resetState)
      ;; Reset to the original domain state, including all possible values
      (set! _list (append _list _hidden))
      (set! _hidden null)
      (set! _states null))
    
    ))


;; ----------------------------------------------------------------------
;; Solvers
;; ----------------------------------------------------------------------

(define Solver
  ;; Abstract base class for solvers
  (class object%
    (super-new)
    (abstract getSolution)
    (abstract getSolutions)
    (abstract getSolutionIter)))


(define BacktrackingSolver
  ;; Problem solver with backtracking capabilities
  (class Solver
    (super-new)
    (init-field [forwardCheck #t])
    
    (define/override (getSolutionIter domains constraints vconstraints)
      ;; resume here
      (void))
    
    (define/override (getSolution domains constraints vconstraints)
      ;; todo: repair this properly
      (car (getSolutions domains constraints vconstraints)))
    
    (define/override (getSolutions domains constraints vconstraints)
      (getSolutionIter domains constraints vconstraints))
    
    ))


(module+ main
  (define p (new Problem))
  (define d  (new Domain [set '(1 2)]))
  )