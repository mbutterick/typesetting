#lang racket/base

;; Adapted from work by Peter Norvig
;; http://aima-python.googlecode.com/svn/trunk/csp.py

(require racket/list racket/bool racket/contract racket/class racket/match)
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




(define (AC3 csp queue)
  (void))

#|
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
|#

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


#______________________________________________________________________________
# CSP Backtracking Search
                
def backtracking_search(csp, mcv=False, lcv=False, fc=False, mac=False):
    """Set up to do recursive backtracking search. Allow the following options:
    mcv - If true, use Most Constrained Variable Heuristic
    lcv - If true, use Least Constraining Value Heuristic
    fc  - If true, use Forward Checking
    mac - If true, use Maintaining Arc Consistency.              [Fig. 5.3]
    >>> backtracking_search(australia)
    {'WA': 'B', 'Q': 'B', 'T': 'B', 'V': 'B', 'SA': 'G', 'NT': 'R', 'NSW': 'R'}
    """
    if fc or mac:
        csp.curr_domains, csp.pruned = {}, {}
        for v in csp.vars:
            csp.curr_domains[v] = csp.domains[v][:]
            csp.pruned[v] = []
    update(csp, mcv=mcv, lcv=lcv, fc=fc, mac=mac)
    return recursive_backtracking({}, csp)

def recursive_backtracking(assignment, csp):
    """Search for a consistent assignment for the csp.
    Each recursive call chooses a variable, and considers values for it."""
    if len(assignment) == len(csp.vars):
        return assignment
    var = select_unassigned_variable(assignment, csp)
    for val in order_domain_values(var, assignment, csp):
        if csp.fc or csp.nconflicts(var, val, assignment) == 0:
            csp.assign(var, val, assignment)
            result = recursive_backtracking(assignment, csp)
            if result is not None:
                return result
        csp.unassign(var, assignment)
    return None

def select_unassigned_variable(assignment, csp):
    "Select the variable to work on next.  Find"
    if csp.mcv: # Most Constrained Variable 
        unassigned = [v for v in csp.vars if v not in assignment] 
        return argmin_random_tie(unassigned,
                     lambda var: -num_legal_values(csp, var, assignment))
    else: # First unassigned variable
        for v in csp.vars:
            if v not in assignment:
                return v

def order_domain_values(var, assignment, csp):
    "Decide what order to consider the domain variables."
    if csp.curr_domains:
        domain = csp.curr_domains[var]
    else:
        domain = csp.domains[var][:]
    if csp.lcv:
        # If LCV is specified, consider values with fewer conflicts first
        key = lambda val: csp.nconflicts(var, val, assignment)
        domain.sort(lambda(x,y): cmp(key(x), key(y)))
    while domain:
        yield domain.pop()

def num_legal_values(csp, var, assignment):
    if csp.curr_domains:
        return len(csp.curr_domains[var])
    else:
        return count_if(lambda val: csp.nconflicts(var, val, assignment) == 0,
                        csp.domains[var])

#______________________________________________________________________________
# Constraint Propagation with AC-3

def AC3(csp, queue=None):
    """[Fig. 5.7]"""
    if queue == None:
        queue = [(Xi, Xk) for Xi in csp.vars for Xk in csp.neighbors[Xi]]
    while queue:
        (Xi, Xj) = queue.pop()
        if remove_inconsistent_values(csp, Xi, Xj):
            for Xk in csp.neighbors[Xi]:
                queue.append((Xk, Xi))

def remove_inconsistent_values(csp, Xi, Xj):
    "Return true if we remove a value."
    removed = False
    for x in csp.curr_domains[Xi][:]:
        # If Xi=x conflicts with Xj=y for every possible y, eliminate Xi=x
        if every(lambda y: not csp.constraints(Xi, x, Xj, y),
                csp.curr_domains[Xj]):
            csp.curr_domains[Xi].remove(x)
            removed = True
    return removed

#______________________________________________________________________________
# Min-conflicts hillclimbing search for CSPs

def min_conflicts(csp, max_steps=1000000): 
    """Solve a CSP by stochastic hillclimbing on the number of conflicts."""
    # Generate a complete assignement for all vars (probably with conflicts)
    current = {}; csp.current = current
    for var in csp.vars:
        val = min_conflicts_value(csp, var, current)
        csp.assign(var, val, current)
    # Now repeapedly choose a random conflicted variable and change it
    for i in range(max_steps):
        conflicted = csp.conflicted_vars(current)
        if not conflicted:
            return current
        var = random.choice(conflicted)
        val = min_conflicts_value(csp, var, current)
        csp.assign(var, val, current)
    return None

def min_conflicts_value(csp, var, current):
    """Return the value that will give var the least number of conflicts.
    If there is a tie, choose at random."""
    return argmin_random_tie(csp.domains[var],
                             lambda val: csp.nconflicts(var, val, current)) 

#______________________________________________________________________________
# Map-Coloring Problems

class UniversalDict:
    """A universal dict maps any key to the same value. We use it here
    as the domains dict for CSPs in which all vars have the same domain.
    >>> d = UniversalDict(42)
    >>> d['life']
    42
    """
    def __init__(self, value): self.value = value
    def __getitem__(self, key): return self.value
    def __repr__(self): return '{Any: %r}' % self.value

def different_values_constraint(A, a, B, b):
    "A constraint saying two neighboring variables must differ in value."
    return a != b

def MapColoringCSP(colors, neighbors):
    """Make a CSP for the problem of coloring a map with different colors
    for any two adjacent regions.  Arguments are a list of colors, and a
    dict of {region: [neighbor,...]} entries.  This dict may also be
    specified as a string of the form defined by parse_neighbors"""

    if isinstance(neighbors, str):
        neighbors = parse_neighbors(neighbors)     
    return CSP(neighbors.keys(), UniversalDict(colors), neighbors,
               different_values_constraint)

def parse_neighbors(neighbors, vars=[]):
    """Convert a string of the form 'X: Y Z; Y: Z' into a dict mapping
    regions to neighbors.  The syntax is a region name followed by a ':'
    followed by zero or more region names, followed by ';', repeated for
    each region name.  If you say 'X: Y' you don't need 'Y: X'.
    >>> parse_neighbors('X: Y Z; Y: Z')
    {'Y': ['X', 'Z'], 'X': ['Y', 'Z'], 'Z': ['X', 'Y']}
    """
    dict = DefaultDict([])
    for var in vars:
        dict[var] = []
    specs = [spec.split(':') for spec in neighbors.split(';')]
    for (A, Aneighbors) in specs:
        A = A.strip();
        dict.setdefault(A, [])
        for B in Aneighbors.split():
            dict[A].append(B)
            dict[B].append(A)
    return dict

australia = MapColoringCSP(list('RGB'),
                           'SA: WA NT Q NSW V; NT: WA Q; NSW: Q V; T: ')
    
usa = MapColoringCSP(list('RGBY'),
        """WA: OR ID; OR: ID NV CA; CA: NV AZ; NV: ID UT AZ; ID: MT WY UT;
        UT: WY CO AZ; MT: ND SD WY; WY: SD NE CO; CO: NE KA OK NM; NM: OK TX;
        ND: MN SD; SD: MN IA NE; NE: IA MO KA; KA: MO OK; OK: MO AR TX;
        TX: AR LA; MN: WI IA; IA: WI IL MO; MO: IL KY TN AR; AR: MS TN LA;
        LA: MS; WI: MI IL; IL: IN; IN: KY; MS: TN AL; AL: TN GA FL; MI: OH;
        OH: PA WV KY; KY: WV VA TN; TN: VA NC GA; GA: NC SC FL;
        PA: NY NJ DE MD WV; WV: MD VA; VA: MD DC NC; NC: SC; NY: VT MA CA NJ;
        NJ: DE; DE: MD; MD: DC; VT: NH MA; MA: NH RI CT; CT: RI; ME: NH;
        HI: ; AK: """)
#______________________________________________________________________________
# n-Queens Problem

def queen_constraint(A, a, B, b):
    """Constraint is satisfied (true) if A, B are really the same variable,
    or if they are not in the same row, down diagonal, or up diagonal."""
    return A == B or (a != b and A + a != B + b and A - a != B - b)

class NQueensCSP(CSP):
    """Make a CSP for the nQueens problem for search with min_conflicts.
    Suitable for large n, it uses only data structures of size O(n).
    Think of placing queens one per column, from left to right.
    That means position (x, y) represents (var, val) in the CSP.
    The main structures are three arrays to count queens that could conflict:
        rows[i]      Number of queens in the ith row (i.e val == i)
        downs[i]     Number of queens in the \ diagonal
                     such that their (x, y) coordinates sum to i
        ups[i]       Number of queens in the / diagonal
                     such that their (x, y) coordinates have x-y+n-1 = i
    We increment/decrement these counts each time a queen is placed/moved from
    a row/diagonal. So moving is O(1), as is nconflicts.  But choosing
    a variable, and a best value for the variable, are each O(n).
    If you want, you can keep track of conflicted vars, then variable
    selection will also be O(1).
    >>> len(backtracking_search(NQueensCSP(8)))
    8
    >>> len(min_conflicts(NQueensCSP(8)))
    8
    """
    def __init__(self, n):
        """Initialize data structures for n Queens."""
        CSP.__init__(self, range(n), UniversalDict(range(n)),
                     UniversalDict(range(n)), queen_constraint)
        update(self, rows=[0]*n, ups=[0]*(2*n - 1), downs=[0]*(2*n - 1))

    def nconflicts(self, var, val, assignment): 
        """The number of conflicts, as recorded with each assignment.
        Count conflicts in row and in up, down diagonals. If there
        is a queen there, it can't conflict with itself, so subtract 3."""
        n = len(self.vars)
        c = self.rows[val] + self.downs[var+val] + self.ups[var-val+n-1]
        if assignment.get(var, None) == val:
            c -= 3
        return c

    def assign(self, var, val, assignment):
        "Assign var, and keep track of conflicts."
        oldval = assignment.get(var, None)
        if val != oldval:
            if oldval is not None: # Remove old val if there was one
                self.record_conflict(assignment, var, oldval, -1)
            self.record_conflict(assignment, var, val, +1)
            CSP.assign(self, var, val, assignment)

    def unassign(self, var, assignment):
        "Remove var from assignment (if it is there) and track conflicts."
        if var in assignment:
            self.record_conflict(assignment, var, assignment[var], -1)
        CSP.unassign(self, var, assignment)
        
    def record_conflict(self, assignment, var, val, delta):
        "Record conflicts caused by addition or deletion of a Queen."
        n = len(self.vars)
        self.rows[val] += delta
        self.downs[var + val] += delta
        self.ups[var - val + n - 1] += delta

    def display(self, assignment):
        "Print the queens and the nconflicts values (for debugging)."
        n = len(self.vars)
        for val in range(n):
            for var in range(n):
                if assignment.get(var,'') == val: ch ='Q'
                elif (var+val) % 2 == 0: ch = '.'
                else: ch = '-'
                print ch,
            print '    ',
            for var in range(n):
                if assignment.get(var,'') == val: ch ='*'
                else: ch = ' '
                print str(self.nconflicts(var, val, assignment))+ch, 
            print        

#______________________________________________________________________________
# The Zebra Puzzle

def Zebra():
    "Return an instance of the Zebra Puzzle."
    Colors = 'Red Yellow Blue Green Ivory'.split()
    Pets = 'Dog Fox Snails Horse Zebra'.split()
    Drinks = 'OJ Tea Coffee Milk Water'.split()
    Countries = 'Englishman Spaniard Norwegian Ukranian Japanese'.split()
    Smokes = 'Kools Chesterfields Winston LuckyStrike Parliaments'.split()
    vars = Colors + Pets + Drinks + Countries + Smokes
    domains = {}
    for var in vars:
        domains[var] = range(1, 6)
    domains['Norwegian'] = [1]
    domains['Milk'] = [3]
    neighbors = parse_neighbors("""Englishman: Red;
                Spaniard: Dog; Kools: Yellow; Chesterfields: Fox;
                Norwegian: Blue; Winston: Snails; LuckyStrike: OJ;
                Ukranian: Tea; Japanese: Parliaments; Kools: Horse;
                Coffee: Green; Green: Ivory""", vars)
    for type in [Colors, Pets, Drinks, Countries, Smokes]:
        for A in type:
            for B in type:
                if A != B:
                    if B not in neighbors[A]: neighbors[A].append(B)
                    if A not in neighbors[B]: neighbors[B].append(A)
    def zebra_constraint(A, a, B, b, recurse=0):
        same = (a == b)
        next_to = abs(a - b) == 1
        if A == 'Englishman' and B == 'Red': return same
        if A == 'Spaniard' and B == 'Dog': return same
        if A == 'Chesterfields' and B == 'Fox': return next_to
        if A == 'Norwegian' and B == 'Blue': return next_to
        if A == 'Kools' and B == 'Yellow': return same
        if A == 'Winston' and B == 'Snails': return same
        if A == 'LuckyStrike' and B == 'OJ': return same
        if A == 'Ukranian' and B == 'Tea': return same
        if A == 'Japanese' and B == 'Parliaments': return same
        if A == 'Kools' and B == 'Horse': return next_to
        if A == 'Coffee' and B == 'Green': return same
        if A == 'Green' and B == 'Ivory': return (a - 1) == b
        if recurse == 0: return zebra_constraint(B, b, A, a, 1)
        if ((A in Colors and B in Colors) or
            (A in Pets and B in Pets) or
            (A in Drinks and B in Drinks) or
            (A in Countries and B in Countries) or
            (A in Smokes and B in Smokes)): return not same        
        raise 'error'
    return CSP(vars, domains, neighbors, zebra_constraint)

def solve_zebra(algorithm=min_conflicts, **args):
    z = Zebra()
    ans = algorithm(z, **args)
    for h in range(1, 6):
        print 'House', h,
        for (var, val) in ans.items():
            if val == h: print var,
        print
    return ans['Zebra'], ans['Water'], z.nassigns, ans,
               
    
|#
