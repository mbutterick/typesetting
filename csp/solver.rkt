#lang racket/base
(require racket/class sugar/container racket/list racket/generator racket/match "helper.rkt")
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
  (let ([vvp (car vvps)])
    (set! vvps (cdr vvps))
    (values (vvp-variable vvp) (vvp-values vvp) (vvp-pushdomains vvp))))

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
          ;(report lst)
          (let/ec break-for-loop
            (for ([last-item (in-list (map last work-list))]
                  #:when (not (hash-has-key? assignments last-item)))
              ; Found unassigned variable
              (set! variable last-item)
              (set! values ((hash-ref domains variable)))
              (set! pushdomains 
                    (if forwardcheck
                        (for/list ([x (in-hash-keys domains)] 
                                   #:when (and (not (hash-has-key? assignments x)) 
                                               (not (equal? variable x))))
                          (hash-ref domains x))
                        null))   
              (break-for-loop))
            
            ;; if it makes it through the loop without breaking, then there are
            ;; no unassigned variables. We've got a solution. Go back
            ;; to last variable, if there's one.
            (yield (hash-copy assignments))
            (when (null? queue) (begin
                                  (set! want-to-return #t) 
                                  (return-k)))
            (set!-values (variable values pushdomains) (pop-vvp-values! queue))
            (for-each-send pop-state pushdomains))          
          
          (let/ec break-loop2
            (let loop2 ()
              ;; We have a variable. Do we have any values left?
              (when (null? values)                
                ;; No. Go back to last variable, if there's one.
                (hash-remove! assignments variable)
                (let/ec break-loop3
                  (let loop3 ()
                    (if (not (null? queue))
                        (let ()
                          (set!-values (variable values pushdomains) (pop-vvp-values! queue))
                          (for-each-send pop-state pushdomains)
                          (when (not (null? values)) (break-loop3))
                          (hash-remove! assignments variable)
                          (loop3))
                        (begin
                          (set! want-to-return #t) 
                          (return-k))))))
              
              ;; Got a value. Check it.
              (hash-set! assignments variable (py-pop! values))  
              (for-each-send push-state pushdomains)
              (let/ec break-for-loop
                (for ([cvpair (in-list (hash-ref vconstraints variable))])
                  (match-define (list constraint variables) cvpair)
                  (define the_result (send constraint call variables domains assignments pushdomains))
                  (when (not the_result) ; Value is not good.
                    (break-for-loop)))
                (break-loop2))
              
              (for-each-send pop-state pushdomains)
              (loop2)))
          
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
