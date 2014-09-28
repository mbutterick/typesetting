#lang racket/base

;; Adapted from work by Peter Norvig
;; http://aima-python.googlecode.com/svn/trunk/csp.py

(require racket/list racket/bool racket/contract)
(require "csp-utils.rkt" "csp-search.rkt")

#|

class CSP(search.Problem):
    """This class describes finite-domain Constraint Satisfaction Problems.
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

    >>> search.depth_first_graph_search(australia)
    <Node (('WA', 'B'), ('Q', 'B'), ('T', 'B'), ('V', 'B'), ('SA', 'G'), ('NT', 'R'), ('NSW', 'R'))>
    """

|#


(define (init csp vars domains neighbors constraints)
  ;; Construct a CSP problem. If vars is empty, it becomes domains.keys().
  (define vars (if (null? vars) (hash-keys domains) vars))
  (hash-set*! csp 'vars vars 'domains domains
          'neighbors neighbors 'constraints constraints
          'initial null 'curr_domains null 'nassigns 0))

(define (assign csp var val assignment)
  ;; Add {var: val} to assignment; Discard the old value if any.
  (hash-set! assignment var val)
  (hash-update! csp 'nassigns add1))


(define (unassign csp var assignment)
  ;; Remove {var: val} from assignment.
  ;; DO NOT call this if you are changing a variable to a new value;
  ;; just call assign for that.
  (hash-remove! csp var))


(define (nconflicts csp var val assignment)
  ;; Return the number of conflicts var=val has with other variables.
  (define (conflict var2)
    (and (hash-has-key? assignment var2)
         (not ((hash-ref csp 'constraints) var val var2 (hash-ref assignment var2)))))
  (length (filter-not false? (map conflict (hash-ref (hash-ref csp 'neighbors) var)))))

(define (display csp assignment)
  ;; Show a human-readable representation of the CSP.
  (displayln (format "CSP: ~a with assignment: ~a" csp (hash-ref csp assignment))))

(define (actions csp state)
  ;; Return a list of applicable actions: nonconflicting
  ;; assignments to an unassigned variable.
  (if (= (length state) (length (hash-ref csp 'vars)))
      null
      (let () 
        (define assignment (make-hash state))
        (define var (findf (λ(v) (not (hash-has-key? assignment v))) (hash-ref csp 'vars)))
        (map (λ(val) (list var val)) 
             (filter (λ(val) (= 0 (nconflicts csp var val assignment))) (hash-ref (hash-ref csp 'domains) var))))))
  
  
#|

    def actions(self, state):
        """Return a list of applicable actions: nonconflicting
        assignments to an unassigned variable."""
        if len(state) == len(self.vars):
            return []
        else:
            assignment = dict(state)
            var = find_if(lambda v: v not in assignment, self.vars)
            return [(var, val) for val in self.domains[var]
                    if self.nconflicts(var, val, assignment) == 0]

    def result(self, state, (var, val)):
        "Perform an action and return the new state."
        return state + ((var, val),)

    def goal_test(self, state):
        "The goal is to assign all vars, with all constraints satisfied."
        assignment = dict(state)
        return (len(assignment) == len(self.vars) and
                every(lambda var: self.nconflicts(var, assignment[var],
                                                  assignment) == 0,
                      self.vars))

    ## These are for constraint propagation

    def support_pruning(self):
        """Make sure we can prune values from domains. (We want to pay
        for this only if we use it.)"""
        if self.curr_domains is None:
            self.curr_domains = dict((v, list(self.domains[v]))
                                     for v in self.vars)

    def suppose(self, var, value):
        "Start accumulating inferences from assuming var=value."
        self.support_pruning()
        removals = [(var, a) for a in self.curr_domains[var] if a != value]
        self.curr_domains[var] = [value]
        return removals

    def prune(self, var, value, removals):
        "Rule out var=value."
        self.curr_domains[var].remove(value)
        if removals is not None: removals.append((var, value))

    def choices(self, var):
        "Return all values for var that aren't currently ruled out."
        return (self.curr_domains or self.domains)[var]

    def infer_assignment(self):
        "Return the partial assignment implied by the current inferences."
        self.support_pruning()
        return dict((v, self.curr_domains[v][0])
                    for v in self.vars if 1 == len(self.curr_domains[v]))

    def restore(self, removals):
        "Undo a supposition and all inferences from it."
        for B, b in removals:
            self.curr_domains[B].append(b)

    ## This is for min_conflicts search

    def conflicted_vars(self, current):
        "Return a list of variables in current assignment that are in conflict"
        return [var for var in self.vars
                if self.nconflicts(var, current[var], current) > 0]

|#
