#lang racket/base
(require racket/class sugar/unstable/container sugar/debug racket/list 
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
  (if (empty? vvps)
      (error 'pop-vvp-values! (format "~a is null" vvps))
      (let ([vvp (car vvps)])
        (set! vvps (cdr vvps))
        (values (vvp-variable vvp) (vvp-values vvp) (vvp-pushdomains vvp)))))

#|
(define (recursive-backtracking assignment csp)
  (if (complete? assignment) 
      assignment
    (let ([var (select-unassigned-variable csp-variables, assignment, csp)])
      (for/or ([value (in-list (order-domain-values var assignment csp))])
        if ((value . consistent-with? . assignment csp-constraints))
        (add-to assignment var value)
        (define result (recursive-backtracking assignment csp))
        (when result
          (and result (remove-from assignment var value)))
        #f))))
|#

(define backtracking-solver%
  ;; Problem solver with backtracking capabilities
  (class solver%
    (super-new)
    (init-field [forwardcheck #t])
    (field [_forwardcheck forwardcheck]) 
    
    (define/override (get-solution-iter domains constraints vconstraints)
      (define sorted-variables (sort (hash-keys domains) list-comparator
                                      #:key (λ(var)
                                              (list (- (length (hash-ref vconstraints var)))
                                                    (length ((hash-ref domains var)))
                                                    var))))
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
        (forever
         (unless (get-next-unassigned-variable)
           (yield (hash-copy possible-solution)) ; if there are no unassigned variables, solution is complete.
           (if (empty? variable-queue)
               (exit-k) ; all done, no other solutions possible.
               (set!-previous-variable))) ; otherwise return to previous variable
         
         (let value-checking-loop () ; we have a variable. Do we have any values left?
           (when (empty? values) ; no, so try going back to last variable and getting some values
             (forever/until
              (when (empty? variable-queue) (exit-k)) ; no variables left, so solver is done
              (hash-remove! possible-solution variable)
              (set!-previous-variable)
              (not (empty? values))))        
           
           ;; Got a value. Check it.
           (hash-set! possible-solution variable (car-pop! values))
           (for-each-send push-state pushdomains)
           (unless (for/and ([constraint+variables (in-list (hash-ref vconstraints variable))])
                     (let ([constraint (car constraint+variables)]
                           [variables (cadr constraint+variables)])
                       (send constraint is-true? variables domains possible-solution pushdomains)))
             ;; constraint failed, so try again
             (for-each-send pop-state pushdomains) 
             (value-checking-loop)))
         
         ;; Push state before looking for next variable.
         (set! variable-queue (cons (vvp variable values pushdomains) variable-queue)))
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
