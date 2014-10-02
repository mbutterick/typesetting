#lang racket/base
(require racket/class sugar/container racket/list racket/generator racket/match "helper.rkt")
(provide (all-defined-out))

(define Solver
  ;; Abstract base class for solvers
  (class object%
    (super-new)
    (abstract getSolution)
    (abstract getSolutions)
    (abstract getSolutionIter)))

(define Solver? (is-a?/c Solver))

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
          ;(displayln "starting while loop 1")
          
          
          ;; Mix the Degree and Minimum Remaing Values (MRV) heuristics
          (set! lst (sort (for/list ([variable (in-hash-keys domains)])
                            (list (* -1 (length (hash-ref vconstraints variable)))
                                  (length (get-field _list (hash-ref domains variable)))
                                  variable)) list-comparator))
          ;(report lst)
          (let/ec break-for-loop
            (for ([item (in-list lst)])
              (when (not ((last item) . in? . assignments))
                
                ; Found unassigned variable
                (set! variable (last item))
                ;(report variable unassigned-variable)
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
            (yield (hash-copy assignments))
            (when (null? queue) (begin
                                  (set! want-to-return #t) 
                                  (return-k)))
            (define variable-values-pushdomains (py-pop! queue))
            (set! variable (first variable-values-pushdomains))
            (set-field! _list values (second variable-values-pushdomains))
            (set! pushdomains (third variable-values-pushdomains))
            (for ([domain (in-list pushdomains)])
              (send domain popState)))          
          
          ;(report variable variable-preloop-2)
          ;(report assignments assignments-preloop-2)
          
          (let/ec break-loop2
            (let loop2 ()
              ;(displayln "starting while loop 2")
              
              ;; We have a variable. Do we have any values left?
              ;(report values values-tested)
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
              (hash-set! assignments variable (send values domain-pop!))  
              
              (for ([domain (in-list pushdomains)])
                (send domain pushState))
              ;(report pushdomains pushdomains1)
              ;(report domains domains1)
              
              (let/ec break-for-loop
                (for ([cvpair (in-list (hash-ref vconstraints variable))])
                  (match-define (list constraint variables) cvpair)
                  (define the_result (send constraint call variables domains assignments pushdomains))
                  ;(report pushdomains pushdomains2)
                  ;(report domains domains2)
                  ;(report the_result)
                  (when (not the_result)
                    ;; Value is not good.
                    (break-for-loop)))
                (begin ;(displayln "now breaking loop 2") 
                  (break-loop2)))
              
              (for ([domain (in-list pushdomains)])
                (send domain popState))
              
              (loop2)))
          
          ;; Push state before looking for next variable.
          (py-append! queue (list variable (get-field _list (send values copy)) pushdomains))
          ;(report queue new-queue)
          (loop1)))
      
      (if want-to-return
          (void)
          (error 'getSolutionIter "Whoops, broken solver")))
    
    
    (define (call-solution-generator domains constraints vconstraints #:first-only [first-only #f])
      (for/list ([solution (in-generator (getSolutionIter domains constraints vconstraints))] #:final first-only) 
        solution))
    
    (define/override (getSolution . args)
      (car (apply call-solution-generator #:first-only #t args)))
    
    (define/override (getSolutions . args)
      (apply call-solution-generator args))
    
    ))
