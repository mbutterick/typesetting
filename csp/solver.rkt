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
      
      (define forwardcheck _forwardcheck)
      (define assignments (make-hash))
      (define queue null)
      (define values null)
      (define pushdomains null)
      (define variable #f)
      (define work-list null)
      (define want-to-return #f)
      (define return-k #f)
      (let/ec break-loop1
        (set! return-k break-loop1)
        (let loop1 ()
          ;; Mix the Degree and Minimum Remaing Values (MRV) heuristics
          (set! work-list (sort (for/list ([variable (in-hash-keys domains)])
                                  (list (* -1 (length (hash-ref vconstraints variable)))
                                        (length ((hash-ref domains variable)))
                                        variable)) list-comparator))
          
          (define found-unassigned-variable?
            (for/first ([last-item (in-list (map last work-list))]
                        #:when (not (hash-has-key? assignments last-item)))
              (set! variable last-item)
              (set! values ((hash-ref domains variable)))
              (set! pushdomains 
                    (if forwardcheck
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
              [else
               (set! want-to-return #t) (return-k)]))          
          
          (let constraint-checking-loop ()
            ;; We have a variable. Do we have any values left?
            (when (null? values)                
              ;; No. Go back to last variable, if there is one, otherwise solver is done.
              (for/or ([i (in-naturals)])
                (hash-remove! assignments variable)
                (when (null? queue) (set! want-to-return #t) (return-k))
                (set!-values (variable values pushdomains) (pop-vvp-values! queue))
                (for-each-send pop-state pushdomains)
                (not (null? values))))              
            
            ;; Got a value. Check it.
            (hash-set! assignments variable (car-pop! values))
            (for-each-send push-state pushdomains)
            (when (for/or ([cvpair (in-list (hash-ref vconstraints variable))])
                    (match-define (list constraint variables) cvpair)
                    (not (send constraint is-true? variables domains assignments pushdomains)))
              ;; constraint failed, so try again
              (for-each-send pop-state pushdomains) 
              (constraint-checking-loop)))
          
          ;; Push state before looking for next variable.
          (set! queue (cons (vvp variable values pushdomains) queue))
          (loop1)))
      
      (if want-to-return
          (void)
          (error 'get-solution-iter "Whoops, broken solver")))
    
    
    (define (call-solution-generator domains constraints vconstraints #:first-only [first-only #f])
      (for/list ([solution (in-generator (get-solution-iter domains constraints vconstraints))] #:final first-only) 
        solution))
    
    (define/override (get-solution . args)
      (car (apply call-solution-generator #:first-only #t args)))
    
    (define/override (get-solutions . args)
      (apply call-solution-generator args))))

(define backtracking-solver%? (is-a?/c backtracking-solver%))
