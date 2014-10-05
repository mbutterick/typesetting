#lang racket/base
(require racket/class sugar/container sugar/debug racket/list racket/generator racket/match "helper.rkt")
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
      (define work-list (sort (for/list ([variable (in-hash-keys domains)])
                                (list (* -1 (length (hash-ref vconstraints variable)))
                                      (length ((hash-ref domains variable)))
                                      variable)) list-comparator))
      ;; state-retention variables
      (define assignments (make-hash))
      (define queue null)
      (define variable #f)
      (define values null)
      (define pushdomains null)
      
      (let/ec exit-k
        (let main-loop ()
          ;; mix the degree and minimum-remaining-values (MRV) heuristics
          (define found-unassigned-variable?
            (for/first ([last-item (in-list (map last work-list))]
                        #:when (not (hash-has-key? assignments last-item)))
              (set! variable last-item)
              (set! values ((hash-ref domains variable)))
              (set! pushdomains 
                    (if _forwardcheck
                        (for/list ([x (in-hash-keys domains)] 
                                   #:when (and (not (hash-has-key? assignments x)) 
                                               (not (equal? variable x))))
                          (hash-ref domains x))
                        null))   
              variable))
          
          ;; if there are no unassigned variables, we've got a solution. 
          (when (not found-unassigned-variable?)
            (yield (hash-copy assignments))
            (cond
              ;; Return to previous variable in queue if possible, otherwise all done
              [(not (null? queue))
               (set!-values (variable values pushdomains) (pop-vvp-values! queue))
               (for-each-send pop-state pushdomains)]
              [else (exit-k)]))          
          
          (let value-checking-loop () ; we have a variable. Do we have any values left?
            (when (null? values) ; no, so try going back to last variable and getting some values
              (for/or ([i (in-naturals)])
                (when (null? queue) (exit-k)) ; no variables left, so solver is done
                (hash-remove! assignments variable)
                (set!-values (variable values pushdomains) (pop-vvp-values! queue))
                (for-each-send pop-state pushdomains)
                (not (null? values))))        
            
            ;; Got a value. Check it.
            (hash-set! assignments variable (car-pop! values))
            (for-each-send push-state pushdomains)
            (when (for/or ([cvpair (in-list (hash-ref vconstraints variable))])
                    (match-define (list constraint variables) cvpair)
                    (not (send constraint broken? variables domains assignments pushdomains)))
              ;; constraint failed, so try again
              (for-each-send pop-state pushdomains) 
              (value-checking-loop)))
          
          ;; Push state before looking for next variable.
          (set! queue (cons (vvp variable values pushdomains) queue))
          (main-loop))
        (error 'get-solution-iter "Should never get here"))      
      (void))
    
    
    (define (call-solution-generator domains constraints vconstraints #:first-only [first-only #f])
      (for/list ([solution (in-generator (get-solution-iter domains constraints vconstraints))] #:final first-only) 
        solution))
    
    (define/override (get-solution . args)
      (car (apply call-solution-generator #:first-only #t args)))
    
    (define/override (get-solutions . args)
      (apply call-solution-generator args))))

(define backtracking-solver%? (is-a?/c backtracking-solver%))
