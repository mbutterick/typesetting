#lang racket/base
(require racket/class racket/match)

(provide (all-defined-out))

(define Problem
  ;; The abstract class for a formal problem.  You should subclass this and
  ;; implement the method successor, and possibly __init__, goal_test, and
  ;; path_cost. Then you will create instances of your subclass and solve them
  ;; with the various search functions.
  
  (class object%
    (super-new)
    
    (init-field initial [goal #f])
    ;; The constructor specifies the initial state, and possibly a goal
    ;; state, if there is a unique goal.  Your subclass's constructor can add
    ;; other arguments.
    
    (abstract successor)
    ;; Given a state, return a sequence of (action, state) pairs reachable
    ;; from this state. If there are many successors, consider an iterator
    ;; that yields the successors one at a time, rather than building them
    ;;  all at once. Iterators will work fine within the framework.
    
    (define/public (goal_test state)
      ;; Return True if the state is a goal. The default method compares the
      ;; state to self.goal, as specified in the constructor. Implement this
      ;; method if checking against a single self.goal is not enough.
      (and (equal? state goal) #t))
    
    (define/public (path_cost c state1 action state2)
      ;; Return the cost of a solution path that arrives at state2 from
      ;; state1 via action, assuming cost c to get up to state1. If the problem
      ;; is such that the path doesn't matter, this function will only look at
      ;; state2.  If the path does matter, it will consider c and maybe state1
      ;; and action. The default method costs 1 for every step in the path.
      (add1 c))
    
    (abstract value)
    ;; For optimization problems, each state has a value.  Hill-climbing
    ;; and related algorithms try to maximize this value.
    ))

(require describe)

(define Node
  #| A node in a search tree. Contains a pointer to the parent (the node
    that this is a successor of) and to the actual state for this node. Note
    that if a state is arrived at by two paths, then there are two nodes with
    the same state.  Also includes the action that got us to this state, and
    the total path_cost (also known as g) to reach the node.  Other functions
    may add an f and h value; see best_first_graph_search and astar_search for
    an explanation of how the f and h values are handled. You will not need to
    subclass this class.
|#
  
  (class* object% (printable<%>)
    (super-new)
    
    (init-field state [parent #f] [action #f] [path_cost 0])
    (field [depth (if parent (add1 (get-field depth parent)) 0)])
    ;; Create a search tree Node, derived from a parent by an action.
    
    (define (repr) (format "<Node ~v>" (get-field state this)))
    (define/public (custom-print out quoting-depth) (print (repr) out))
    (define/public (custom-display out) (displayln (repr) out))
    (define/public (custom-write out) (write (repr) out))
    
    (define/public (path)
      ;; Create a list of nodes from the root to this node.
      (let ([parent (get-field parent this)])
        (cons this 
              (if (not parent)
                  null
                  (send parent path)))))
    
    (define/public (expand problem)
      ;; Return a list of nodes reachable from this node.
      (for/list ([action-state-pair (in-list (send problem successor state))])
        (match-define (cons act next) action-state-pair)
        (new Node [state next][parent this][action act]
             [path_cost (send problem path_cost path_cost state act next)])))    
    ))

(module+ main
  (require racket/format)  
  (define gp (new Node [state 'grandparent]))
  (define p (new Node [state 'parent][parent gp]))
  (get-field state p)
  (get-field depth p)
  (define c (new Node [state 'child] [parent p]))
  (get-field depth c)
  (send c path))