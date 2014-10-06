#lang racket/base
(require racket/class sugar/container sugar/debug racket/list 
         racket/bool racket/generator racket/match "helper.rkt")
(provide (all-defined-out))

(define solver%
  ;; Abstract base class for solvers
  (class object%
    (super-new)
    (abstract get-solution)
    (abstract get-solutions)
    (abstract get-solution-iter)))

(define solver%? (is-a?/c solver%))

(struct vvp (variable values pushdomains))
(define-syntax-rule (pop-vvp-values! vvps)
  (if (null? vvps)
      (error 'pop-vvp-values! (format "~a is null" vvps))
      (let ([vvp (car vvps)])
        (set! vvps (cdr vvps))
        (values (vvp-variable vvp) (vvp-values vvp) (vvp-pushdomains vvp)))))

(define backtracking-solver%
  ;; Problem solver with backtracking capabilities
  (class solver%
    (super-new)
    (init-field [forwardcheck #t])
    (field [_forwardcheck forwardcheck]) 
    
    (define/override (get-solution-iter domains constraints vconstraints)
      (define sorted-variables 
        (map last (sort (for/list ([(variable domain) (in-hash domains)])
                          (list (- (length (hash-ref vconstraints variable))) ; first two elements used for sorting 
                                (length (domain))
                                variable)) list-comparator)))
      ;; state-retention variables
      (define possible-solution (make-hash))
      (define variable-queue null)
      (define variable #f)
      (define values null)
      (define pushdomains null)
      
      (define (get-next-unassigned-variable)
        (for/first ([sorted-variable (in-list sorted-variables)]
                    #:unless (hash-has-key? possible-solution sorted-variable))
          (set! variable sorted-variable)
          (set! values ((hash-ref domains variable)))
          (set! pushdomains 
                (if _forwardcheck
                    (for/list ([(var domain) (in-hash domains)] 
                               #:unless (and (equal? variable var)
                                             (hash-has-key? possible-solution var)))
                      domain)
                    null))   
          variable))
      
      (define (set!-previous-variable)
        (set!-values (variable values pushdomains) (pop-vvp-values! variable-queue))
        (for-each-send pop-state pushdomains))
      
      (let/ec exit-k
        ;; mix the degree and minimum-remaining-values (MRV) heuristics
        (let main-loop ()
          (unless (get-next-unassigned-variable)
            (yield (hash-copy possible-solution)) ; if there are no unassigned variables, solution is done. 
            (if (null? variable-queue) ; if queue isn't empty, return to previous variable, otherwise all done.
                (exit-k)
                (set!-previous-variable)))
          
          (let value-checking-loop () ; we have a variable. Do we have any values left?
            (when (null? values) ; no, so try going back to last variable and getting some values
              (for/or ([i (in-naturals)])
                (when (null? variable-queue) (exit-k)) ; no variables left, so solver is done
                (hash-remove! possible-solution variable)
                (set!-previous-variable)
                (not (null? values))))        
            
            ;; Got a value. Check it.
            (hash-set! possible-solution variable (car-pop! values))
            (for-each-send push-state pushdomains)
            (when (for/or ([cvpair (in-list (hash-ref vconstraints variable))])
                    (match-define (list constraint variables) cvpair)
                    (not (send constraint broken? variables domains possible-solution pushdomains)))
              ;; constraint failed, so try again
              (for-each-send pop-state pushdomains) 
              (value-checking-loop)))
          
          ;; Push state before looking for next variable.
          (set! variable-queue (cons (vvp variable values pushdomains) variable-queue))
          (main-loop))
        (error 'get-solution-iter "impossible to reach this"))      
      (void))
    
    
    (define (call-solution-generator domains constraints vconstraints #:first-only [first-only #f])
      (for/list ([solution (in-generator (get-solution-iter domains constraints vconstraints))] #:final first-only) 
        solution))
    
    (define/override (get-solution . args)
      (car (apply call-solution-generator #:first-only #t args)))
    
    (define/override (get-solutions . args)
      (apply call-solution-generator args))))

(define backtracking-solver%? (is-a?/c backtracking-solver%))
