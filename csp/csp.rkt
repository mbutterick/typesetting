#lang racket/base

;; Adapted from work by Peter Norvig
;; http://aima-python.googlecode.com/svn/trunk/csp.py

(require racket/list racket/bool racket/contract racket/class racket/match racket/generator)
(require "utils.rkt" "search.rkt")

(define CSP (class Problem
              #|
This class describes finite-domain Constraint Satisfaction Problems.
    A CSP is specified by the following inputs:
        vars        A list of variables; each is atomic (e.g. int or string).
        domains     A dict of {var:[possible_value, ...]} entries.
        neighbors   A dict of {var:[var,...]} that for each variable lists
                    the other variables that participate in constraints.
        constraints A function f(A, a, B, b) that returns true if neighbors
                    A, B satisfy the constraint when they have values A=a, B=b
    In the textbook and in most mathematical definitions, the
    constraints are specified as explicit pairs of allowable values,
    but the formulation here is easier to express and more compact for
    most cases. (For example, the n-Queens problem can be represented
    in O(n) space using this notation, instead of O(N^4) for the
    explicit representation.) In terms of describing the CSP as a
    problem, that's all there is.

    However, the class also supports data structures and methods that help you
    solve CSPs by calling a search function on the CSP.  Methods and slots are
    as follows, where the argument 'a' represents an assignment, which is a
    dict of {var:val} entries:
        assign(var, val, a)     Assign a[var] = val; do other bookkeeping
        unassign(var, a)        Do del a[var], plus other bookkeeping
        nconflicts(var, val, a) Return the number of other variables that
                                conflict with var=val
        curr_domains[var]       Slot: remaining consistent values for var
                                Used by constraint propagation routines.
    The following methods are used only by graph_search and tree_search:
        actions(state)          Return a list of actions
        result(state, action)   Return a successor of state
        goal_test(state)        Return true if all constraints satisfied
    The following are just for debugging purposes:
        nassigns                Slot: tracks the number of assignments made
        display(a)              Print a human-readable representation
|#
              (super-new)
              
              ;; Construct a CSP problem. If vars is empty, it becomes domains.keys().
              (init-field vars domains neighbors constraints)
              (when (not vars) (set! vars (hash-keys domains)))
              (inherit-field initial)
              (set! initial (hash))
              (field [curr_domains #f][pruned #f][nassigns 0][fc #f][mac #f])
              
              (define/public (assign var val assignment)
                ;; Add {var: val} to assignment; Discard the old value if any.
                ;; Do bookkeeping for curr_domains and nassigns.
                (set! nassigns (add1 nassigns))
                (hash-set! assignment var val)
                (if curr_domains
                    (when fc
                      (forward_check var val assignment))
                    (when mac
                      (AC3 (map (λ(Xk) (cons Xk var)) (hash-ref neighbors var))))))
              
              (define/public (unassign var val assignment)
                ;; Remove {var: val} from assignment; that is backtrack.
                ;; DO NOT call this if you are changing a variable to a new value;
                ;; just call assign for that.
                (when (hash-has-key? assignment var)
                  ;; Reset the curr_domain to be the full original domain
                  (when curr_domains
                    (hash-set! curr_domains var (hash-ref domains var)))
                  (hash-remove! assignment var)))
              
              (define/public (nconflicts var val assignment)
                ;; Return the number of conflicts var=val has with other variables.
                ;; Subclasses may implement this more efficiently
                (define (conflict var2)
                  (define val2 (hash-ref assignment var2 #f))
                  (and val2 (not (constraints var val var2 val2))))
                (count_if conflict (hash-ref neighbors var)))
              
              (define/public (forward_check var val assignment)
                ;; Do forward checking (current domain reduction) for this assignment.
                (when curr_domains
                  ;; Restore prunings from previous value of var
                  (for ([Bb-pair (in-list (hash-ref pruned var))])
                    (match-define (cons B b) Bb-pair)
                    (hash-update! curr_domains B (λ(v) (append v b))))
                  (hash-set! pruned var #f)
                  ;; Prune any other B=b assignment that conflicts with var=val
                  (for ([B (in-list (hash-ref neighbors var))])
                    (when (not (hash-has-key? assignment B))
                      (for ([b (in-list (hash-ref curr_domains B))])
                        (when (not (constraints var val B b))
                          (hash-update! curr_domains B (λ(v) (remove v b)))
                          (hash-update! pruned var (λ(v) (append v (cons B b))))))))))
              
              (define/public (display assignment)
                ;; Show a human-readable representation of the CSP.
                (displayln (format "CSP: ~a with assignment: ~a" this assignment)))
              
              ;; These methods are for the tree and graph search interface:
              
              (define/public (succ assignment)
                ;; Return a list of (action, state) pairs
                (if (= (length assignment) (length vars))
                    null
                    (let ([var (find_if (λ(v) (not (hash-has-key? assignment v))) vars)])
                      (for/list ([val (in-list (hash-ref domains var))] #:when (= (nconflicts var val assignment) 0))
                        (define a (hash-copy assignment))
                        (hash-set! a var val)
                        (cons (cons var val) a)))))
              
              (define/override (goal_test assignment)
                ;; The goal is to assign all vars, with all constraints satisfied.
                (and (= (length assignment) (length vars))
                     (every (λ(var) (= (nconflicts var (hash-ref assignment var) assignment) 0)) vars)))
              
              ;; This is for min_conflicts search
              (define/public (conflicted_vars current)
                ;; Return a list of variables in current assignment that are in conflict
                (for/list ([var (in-list vars)] 
                           #:when (> (nconflicts var (hash-ref current var) current) 0))
                  var))
              ))

;;______________________________________________________________________________
;; CSP Backtracking Search

(define (backtracking_search csp [mcv #f] [lcv #f] [fc #f] [mac #f])
  #|
Set up to do recursive backtracking search. Allow the following options:
    mcv - If true, use Most Constrained Variable Heuristic
    lcv - If true, use Least Constraining Value Heuristic
    fc  - If true, use Forward Checking
    mac - If true, use Maintaining Arc Consistency.              [Fig. 5.3]
    >>> backtracking_search(australia)
    {'WA': 'B', 'Q': 'B', 'T': 'B', 'V': 'B', 'SA': 'G', 'NT': 'R', 'NSW': 'R'}
|#
  (when (or fc mac)
    (set-field! curr_domains csp (hash))
    (set-field! pruned csp (hash)))
  (set-field! mcv csp mcv)
  (set-field! lcv csp lcv)
  (set-field! fc csp fc)
  (set-field! mac csp mac))

(define (recursive_backtracking assignment csp)
  ;; Search for a consistent assignment for the csp.
  ;; Each recursive call chooses a variable, and considers values for it.
  (cond
    [(= (length assignment) (length (get-field vars csp))) assignment]
    [else
     (define var (select_unassigned_variable assignment csp))
     (define result null)
     (let/ec done ;; sneaky way of getting return-like functionality
       (for ([val (in-list (order_domain_values var assignment csp))])
         (when (or (get-field fc csp) (= (send csp nconflicts var val assignment) 0))
           (send csp assign var val assignment)
           (set! result (recursive_backtracking assignment csp))
           (when (not (null? result)) 
             (done))
           (send csp unassign var assignment)))
       result)]))


(define (select_unassigned_variable assignment csp)
  ;; Select the variable to work on next.  Find
  (if (get-field mcv csp) ; most constrained variable
      (let ()
        (define unassigned (filter (λ(v) (not (hash-has-key? assignment v))) (get-field vars csp)))
        (argmin_random_tie unassigned (λ(var) (* -1 (num_legal_values csp var assignment)))))
      ;; else first unassigned variable
      (for/first ([v (in-list (get-field vars csp))] #:when (not (hash-has-key? assignment v)))
        v)))

(define (order_domain_values var assignment csp)
  ;; Decide what order to consider the domain variables.
  (define domain (if (get-field curr_domains csp) 
                     (hash-ref (get-field curr_domains csp) var)
                     (hash-ref (get-field domains csp) var)))
  (when (get-field lcv csp)
    ;; If LCV is specified, consider values with fewer conflicts first
    (define key (λ(val) (send csp nconflicts var val assignment)))
    (set! domain (sort domain < #:key key)))
  (generator ()
             (let loop ([niamod (reverse domain)])
               (yield (car niamod))
               (loop (cdr niamod)))))

(define (num_legal_values csp var assignment)
  (if (get-field curr_domains csp)
      (length (hash-ref (get-field curr_domains csp) var))
      (count_if (λ(val) (= (send csp nconflicts var val assignment) 0)) (hash-ref (get-field domains csp) var))))


;;______________________________________________________________________________
;; Constraint Propagation with AC-3


(define (AC3 csp [queue null])
  (when (null? queue)
    (set! queue (for*/list ([Xi (in-list (get-field vars csp))] 
                            [Xk (in-list (hash-ref (get-field neighbors csp) Xi))])
                  (cons Xi Xk))))
  (let loop ([eueuq (reverse queue)])
    (when (not (null? eueuq))
      (match-define (cons Xi Xj) (car eueuq))
      (set! eueuq (cdr eueuq)) ;; equivalent to python pop
      (when (remove_inconsistent_values csp Xi Xj)
        (set! eueuq
              (append 
               (reverse (for/list ([Xk (in-list (hash-ref (get-field neighbors csp) Xi))])
                          (cons Xk Xi))) 
               eueuq)))
      (loop eueuq))))

(define (remove_inconsistent_values csp Xi Xj)
  ;; Return true if we remove a value.
  (define removed #f)
  (for ([x (in-list (hash-ref (get-field curr_domains csp) Xi))])
    ;; If Xi=x conflicts with Xj=y for every possible y, eliminate Xi=x
    (when (every (λ(y) (not (send csp constraints Xi x Xj y))) 
                 (hash-ref (get-field curr_domains csp) Xj))
      (hash-update! (get-field curr_domains csp) Xi (λ(val) (remove val x)))
      (set! removed #t)))
  removed)

;;______________________________________________________________________________
;; Min-conflicts hillclimbing search for CSPs

(define (min_conflicts csp [max_steps 1000000])
  ;; Solve a CSP by stochastic hillclimbing on the number of conflicts.
  ;; Generate a complete assignment for all vars (probably with conflicts)
  (define current (hash))
  (set-field! current csp current)
  (for ([var (in-list (get-field vars csp))])
    (define val (min_conflicts_value csp var current))
    (send csp assign var val current))
  ;; Now repeatedly choose a random conflicted variable and change it
  (define found-result #f)
  (let/ec done ;; sneaky way of getting return-like functionality
    (for ([i (in-range max_steps)])
      (define conflicted (send csp conflicted_vars current))
      (when (not conflicted) (set! found-result #t) (done))
      (define var (list-ref conflicted (random (length conflicted))))
      (define val (min_conflicts_value csp var current))
      (send csp assign var val current)))
  (and found-result current))
  
 (define (min_conflicts_value csp var current)
   ;; Return the value that will give var the least number of conflicts.
   ;; If there is a tie, choose at random.
   (argmin_random_tie (hash-ref (get-field domains csp) var)
                      (λ(val) (send csp nconflicts var val current))))


   