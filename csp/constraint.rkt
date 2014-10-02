#lang racket/base
(require racket/class racket/contract racket/match racket/list racket/generator)
(require sugar/container sugar/debug)
(require "helpers.rkt")
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
  
  (define problem (new Problem)) ;; test from line 125
  (send problem addVariable "ab" '(1 2))
  (send problem addVariable "c" '(3))
  ;  (check-equal? (get-field _list (hash-ref (get-field _variables problem) "a")) '(1)) 
  
  (displayln (format "The solution to ~a is ~a" 
                     problem 
                     (send problem getSolutions)))
  
  
  (send problem reset)
  (check-equal? (get-field _variables problem) (make-hash))
  (send problem addVariables '("a" "b") '(1 2 3))
  (check-equal? (get-field _list (hash-ref (get-field _variables problem) "a")) '(1 2 3))
  (check-equal? (get-field _list (hash-ref (get-field _variables problem) "b")) '(1 2 3))
  
  )


;; ----------------------------------------------------------------------
;; Domains
;; ----------------------------------------------------------------------


(define Domain
  ;; Class used to control possible values for variables
  ;; When list or tuples are used as domains, they are automatically
  ;; converted to an instance of that class.
  
  (class* object% (printable<%>)
    (super-new)
    (init-field set)
    (field [_list set][_hidden null][_states null])
    
    (define (repr) (format "<Domain ~v>" _list))
    (define/public (custom-print out quoting-depth) (print (repr) out))
    (define/public (custom-display out) (displayln (repr) out))
    (define/public (custom-write out) (write (repr) out))
    
    (define/public (resetState)
      ;; Reset to the original domain state, including all possible values
      (py-extend! _list _hidden)
      (set! _hidden null)
      (set! _states null))
    
    (define/public (pushState)
      ;; Save current domain state
      ;; Variables hidden after that call are restored when that state
      ;;  is popped from the stack.
      (py-append! _states (length _list)))
    
    (define/public (popState)
      ;; Restore domain state from the top of the stack

      ;; Variables hidden since the last popped state are then available
      ;; again.
      (define diff (- (py-pop! _states) (length _list)))
      (when (not (= 0 diff))
        (py-extend! _list (take-right _hidden diff))
        (set! _hidden (take _hidden (- (length _hidden) diff)))))
    
    (define/public (domain-pop!)
      (py-pop! _list))
    
    (define/public (copy)
      (define copied-domain (new Domain [set _list]))
      (set-field! _hidden copied-domain _hidden)
      (set-field! _states copied-domain _states)
      copied-domain)
    
    
    ))

(define Domain? (is-a?/c Domain))

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
    (init-field [forwardcheck #t])
    (field [_forwardcheck forwardcheck]) 
    
    (define/override (getSolutionIter domains constraints vconstraints)
      (define forwardcheck _forwardcheck)
      (define assignments (make-hash))
      (define queue null)
      (define values null)
      (define pushdomains null)
      (define variable #f)
      (define lst null)
      (define want-to-return #f)
      (define return-k #f)
      (let/ec break-loop1
        (set! return-k break-loop1)
        (let loop1 ()
          (displayln "starting while loop 1")
          ;; Mix the Degree and Minimum Remaing Values (MRV) heuristics
          (set! lst (sort (for/list ([variable (in-hash-keys domains)])
                            (list (* -1 (length (hash-ref vconstraints variable)))
                                  (length (get-field _list (hash-ref domains variable)))
                                  variable)) list-comparator)) 
          (report lst)
          (let/ec break-for-loop
            (for ([item (in-list lst)])
              (when (not ((last item) . in? . assignments))
                ; Found unassigned variable
                (set! variable (last item))
                (let ([unassigned-variable variable]) (report unassigned-variable))
                (set! values (send (hash-ref domains variable) copy))
                (set! pushdomains 
                      (if forwardcheck
                          (for/list ([x (in-hash-keys domains)] 
                                     #:when (and (not (x . in? . assignments)) 
                                                 (not (x . equal? . variable))))
                            (hash-ref domains x))
                          null))   
                (break-for-loop)))
            ;; if it makes it through the loop without breaking, then there are
            ;; No unassigned variables. We've got a solution. Go back
            ;; to last variable, if there's one.
            (displayln "solution time")
            (report assignments solution-assignments)
            (yield (hash-copy assignments))
            (report queue)
            (when (null? queue) (begin
                                  (set! want-to-return #t) 
                                  (return-k)))
            (define variable-values-pushdomains (py-pop! queue))
            (set! variable (first variable-values-pushdomains))
            (set-field! _list values (second variable-values-pushdomains))
            (set! pushdomains (third variable-values-pushdomains))
            (when (not (null? pushdomains))
              (for ([domain (in-list pushdomains)])
                (send domain popState))))          
          (report variable variable-preloop-2)
          (report assignments assignments-preloop-2)
          
          (let/ec break-loop2
            (let loop2 ()
              (displayln "starting while loop 2")
              ;; We have a variable. Do we have any values left?
              (displayln (format "values tested ~a" values))
              (when (null? (get-field _list values))
                ;; No. Go back to last variable, if there's one.
                (hash-remove! assignments variable)
                (let/ec break-loop3
                  (let loop3 ()
                    (if (not (null? queue))
                        (let ()
                          (define variable-values-pushdomains (py-pop! queue))
                          (set! variable (first variable-values-pushdomains))
                          (set-field! _list values (second variable-values-pushdomains))
                          (set! pushdomains (third variable-values-pushdomains))
                          (when (not (null? pushdomains))
                            (for ([domain (in-list pushdomains)])
                              (send domain popState)))
                          (when (not (null? (get-field _list values))) (break-loop3))
                          (hash-remove! assignments variable)
                          (loop3))
                        (begin
                          (set! want-to-return #t) 
                          (return-k))))))
              
              ;; Got a value. Check it.
              (report values)
              (hash-set! assignments variable (send values domain-pop!))  
              
              (when (not (null? pushdomains))
                (for ([domain (in-list pushdomains)])
                  (send domain pushState)))
              
              ;; todo: ok replacement for for/else?
              (if (not (null? (hash-ref vconstraints variable))) 
                  (let/ec break-for-loop
                    (for ([cvpair (in-list (hash-ref vconstraints variable))])
                      (match-define (cons constraint variables) cvpair)
                      (when (not (constraint variables domains assignments pushdomains))
                        ;; Value is not good.
                        (break-for-loop))))
                  (begin (displayln "now breaking loop 2") (break-loop2)))
              
              (when (not (null? pushdomains))
                (for ([domain (in-list pushdomains)])
                  (send domain popState)))
              
              (loop2)))
          
          ;; Push state before looking for next variable.
          (py-append! queue (list variable (get-field _list (send values copy)) pushdomains))
          (report queue new-queue)
          
          (loop1)))
      
      (if want-to-return
          (void)
          (error 'getSolutionIter "Whoops, broken solver")))
    
    
    
    (define/override (getSolution domains constraints vconstraints)
      ;; todo: fix this
      (void))
    
    (define/override (getSolutions domains constraints vconstraints)
      (for/list ([solution (in-generator (getSolutionIter domains constraints vconstraints))]) solution))
    
    ))


(module+ main
  (define p (new Problem))
  (define d  (new Domain [set '(1 2)]))
  )