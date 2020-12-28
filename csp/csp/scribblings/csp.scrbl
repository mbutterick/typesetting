#lang scribble/manual

@(require (except-in scribble/eval examples) scribble/example (for-label racket csp graph (except-in math/number-theory permutations)))

@(define my-eval (make-base-eval))
@(my-eval `(require csp racket/list))

@(define-syntax-rule (my-examples ARG ...)
(examples #:label #f #:eval my-eval ARG ...))

@title{Constraint-satisfaction problems (and how to solve them)}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodule[csp]

@margin-note{This package is in development. I make no commitment to maintaining the public interface documented below.}

Simple solvers for simple constraint-satisfaction problems. It uses the forward-checking + conflict-directed backjumping algorithm described in @link["http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.225.3123&rep=rep1&type=pdf"]{@italic{Hybrid Algorithms for the Constraint Satisfaction Problem}} by Patrick Prosser. Plus other improvements of my own devising.

@section{Installation & usage}

At the command line:
@verbatim{raco pkg install csp}

After that, you can update the package like so:
@verbatim{raco pkg update csp}

Import into your program like so:
@verbatim{(require csp)}

@section{Introduction}

A @deftech{constraint-satisfaction problem} (often shortened to @deftech{CSP}) has two ingredients. The first is a set of @deftech{variables}, each associated with a set of possible values (called its @deftech{domain}). The other is a set of @deftech{constraints} — a fancy word for @italic{rules} — that describe relationships among the variables. 

When we select a value for each variable, we have what's known as an @deftech{assignment} or a @deftech{state}. Solving a CSP means finding an assignment that @deftech{satisfies} all the constraints. A CSP may have any number of solution states (including zero). 

Even if the name is new, the idea of a CSP is probably familiar. For instance, many brain teasers — like Sudoku or crosswords or logic puzzles — are really just constraint-satisfaction problems. (Indeed, you can use this package to ruin all of them.) 

When the computer solves a CSP, it's using an analogous process of deductive reasoning to eliminate impossible assignments, eventually converging on a solution (or determining that no solution exists). 

@section{So this is the ultimate tool for the lazy programmer?}

It allows us to describe a problem to the computer in higher-level terms than we usually do. That can be helpful when we have no idea how to create a specialized algorithm, or we just don't feel like it. 

But there's still some finesse and artistry involved in setting up the CSP, especially its constraints. In general, a CSP with more constraints will converge on a solution faster. Furthermore, since we're not just lazy but also impatient, we usually want our answer in a few seconds, not tomorrow or next week. So it's usually worth spending a little extra effort to specify the constraints as carefully as we can, to maximize our chances of getting an answer in a reasonable time.


@section{First example}

Suppose we wanted to find @link["http://www.friesian.com/pythag.htm"]{Pythagorean triples} with sides between 10 and 49, inclusive.

First we create a new CSP called @racket[triples], using @racket[make-csp]:

@examples[#:label #f #:eval my-eval
(define triples (make-csp))
]


We use CSP variables to represent the values in the triple. We insert each one with @racket[add-var!], where each variable has a @tech{symbol} for its name and a list of values for its domain:

@examples[#:label #f #:eval my-eval
(add-var! triples 'a (range 10 50))
(add-var! triples 'b (range 10 50))
(add-var! triples 'c (range 10 50))
]

Then we need our constraint. We make a function called @racket[valid-triple?] that tests three values to see if they qualify as a Pythagorean triple. Then we insert this function as a constraint using @racket[add-constraint!], passing as arguments 1) the function we want to use for the constraint, and 2) a list of variable names that the constraint applies to. 

@examples[#:label #f #:eval my-eval
(define (valid-triple? x y z)
  (= (expt z 2) (+ (expt x 2) (expt y 2))))

(add-constraint! triples valid-triple? '(a b c))
]

Notice that the argument names used within the constraint function (@racket[x] @racket[y] @racket[z]) have nothing to do with the CSP variable names that are passed to the function @racket['(a b c)]. This makes sense — we might want constraints that apply the same function to different groups of CSP variables. What's important is that the @tech{arity} of the constraint function matches the number of variable names, and that the variable names are ordered correctly (the first variable will become the first argument to the constraint function, and so on).

Finally we call @racket[solve], which finds a solution (if it exists):

@examples[#:label #f #:eval my-eval
(solve triples)
]

``But that's just the 5--12--13 triple, doubled.'' True. Suppose we want to ensure that the values in our solution have no common factors. We add a new @racket[coprime?] constraint:

@examples[#:label #f #:eval my-eval
(require math/number-theory)
(add-constraint! triples coprime? '(a b c))
]

We @racket[solve] again to see the new result:

@examples[#:label #f #:eval my-eval
(solve triples)
]

Perhaps we're curious to see how many of these triples exist. We use @racket[solve*] to find all four solutions:

@examples[#:label #f #:eval my-eval
(solve* triples)
]

``But really there's only two solutions — the values for @racket[a] and @racket[b] are swapped in the other two.'' Fair enough. We might say that this problem is @deftech{symmetric} relative to variables @racket[a] and @racket[b], because they have the same domains and are constrained the same way. We can break the symmetry by adding a constraint that forces @racket[a] to be less than or equal to @racket[b]:

@examples[#:label #f #:eval my-eval
(add-constraint! triples <= '(a b))

(solve* triples)
]

Now our list of solutions doesn't have any symmetric duplicates. 

By the way, what if we had accidentally included @racket[c] in the last constraint? 

@examples[#:label #f #:eval my-eval
(add-constraint! triples <= '(a b c))

(solve* triples)
]

Nothing changes. Why not? Because of the existing @racket[valid-triple?] constraint, @racket[c] is necessarily going to be larger than @racket[a] and @racket[b]. So it always meets this constraint too. It's good practice to not duplicate constraints between the same sets of variables — the ``belt and suspenders'' approach just adds work for no benefit.

We should use @racket[solve*] with care. It can't finish until the CSP solver examines every possible assignment of values in the problem, which can be a big number. Specifically, it's the product of the domain sizes of each variable, which in this case is 40 × 40 × 40 = 64,000. This realm of possible assignments is also known as the CSP's @deftech{state space}. We can also get this number from @racket[state-count]:

@examples[#:label #f #:eval my-eval
(state-count triples)
]

It's easy for a CSP to have a state count in the zillions. For this reason we can supply @racket[solve*] with an optional argument that will only generate a certain number of solutions: 

@examples[#:label #f #:eval my-eval
(time (solve* triples))
(time (solve* triples 2))
]

Here, the answers are the same. But the second call to @racket[solve*] finishes sooner, because it quits as soon as it's found two solutions.

Of course, even when we use ordinary @racket[solve], we don't know how many assignments it will have to try before it finds a solution. If the problem is impossible, even @racket[solve] will have to visit the entire state space before it knows for sure. For instance, let's see what happens if we add a constraint that's impossible to meet:

@examples[#:label #f #:eval my-eval
(add-constraint! triples = '(a b c))

(solve triples)
]

Disappointing but accurate.

The whole example in one block:

@racketblock[
(require csp)

(define triples (make-csp))

(add-var! triples 'a (range 10 50))
(add-var! triples 'b (range 10 50))
(add-var! triples 'c (range 10 50))

(define (valid-triple? x y z)
  (= (expt z 2) (+ (expt x 2) (expt y 2))))
(add-constraint! triples valid-triple? '(a b c))

(require math/number-theory)
(add-constraint! triples coprime? '(a b c))

(add-constraint! triples <= '(a b))

(solve* triples 2)
]

@section{Interlude}

``Dude, are you kidding me? I can write a much shorter loop to do the same thing—"

@my-examples[
(for*/list ([a (in-range 10 50)]
            [b (in-range 10 50)]
            #:when (<= a b)
            [c (in-range 10 50)]
            #:when (and (coprime? a b c) (valid-triple? a b c)))
           (map cons '(a b c) (list a b c)))
]

Yes, I agree that in this toy example, the CSP approach is overkill. The variables are few enough, the domains small enough, and the constraints simple enough, that a loop is more concise. Also, with only 64,000 possibilities in the state space, this sort of brute-force approach is cheap & cheerful.

@section{Second example}

But what about a more complicated problem — like a Sudoku? A Sudoku has 81 squares, each of which can hold the digits 1 through 9. The goal in Sudoku is to fill the grid so that no row, no column, and no ``box'' (a 3 × 3 subgroup of cells) has a duplicate digit. About 25 of the squares are filled in at the start, so the size of the state space is therefore:

@my-examples[
(expt 9 (- 81 25))
]


Well over a zillion, certainly. Let's optimistically suppose that the 3.7GHz processor in your computer takes one cycle to check an assignment. There are 31,557,600 seconds in a year, so the brute-force method will only take this many years:

@my-examples[
(define states (expt 9 (- 81 25)))
(define states-per-second (* 3.7 1e9))
(define seconds-per-year 31557600)
(/ states states-per-second seconds-per-year)
]

@racketmod[
#:file "sudoku.rkt"
racket
(require csp)

(define (make-base-sudoku)
  (define sudoku (make-csp))

  (define cells (range 81))
  (add-vars! sudoku cells (range 1 10))

  (for ([i 9])
    (define row-cells (filter (λ (cell) (= (quotient cell 9) i)) cells))
    (add-all-diff-constraint! sudoku row-cells)
    
    (define col-cells (filter (λ (cell) (= (remainder cell 9) i)) cells))
    (add-all-diff-constraint! sudoku col-cells))

  (define box-starts '(0 3 6 27 30 33 54 57 60))
  (define box-offsets '(0 1 2 9 10 11 18 19 20))
  (for ([start box-starts])
    (add-all-diff-constraint! sudoku (map (curry + start) box-offsets)))
  
  sudoku)

(define (make-sudoku-board . strs)
  (define sudoku (make-base-sudoku))
  (define vals (for*/list ([str (in-list strs)]
                           [c (in-string str)]
                           #:unless (memv c '(#\- #\|)))
                 (string->number (string c))))
  (for ([(val vidx) (in-indexed vals)]
        #:when val)
    (add-constraint! sudoku (curry = val) (list vidx)))
  sudoku)

(current-inference forward-check)
(current-select-variable mrv-degree-hybrid)
(current-order-values shuffle)
(current-node-consistency #t)
(current-arity-reduction #t)

(solve (make-sudoku-board
        "  8|   | 45"
        "   | 8 |9  "
        "  2|4  |   "
        "-----------"
        "5  |  1|76 "
        " 1 | 7 | 8 "
        " 79|5  |  1"
        "-----------"
        "   |  7|4  "
        "  7| 6 |   "
        "65 |   |3  "))
]


@section{Another interlude}

``Dude, are you serious? The JMAXX Sudoku Solver runs three to four times faster—''

@racketblock[
;; TK
]

Yes, I agree that an algorithm custom-tailored to the problem will likely beat the CSP solver, which is necessarily general-purpose. 

But let's consider the labor involved. To write something like the JMAXX Sudoku Solver, we'd need a PhD in computer science, and the time to explain not just the rules of Sudoku to the computer, but the process for solving a Sudoku.

By contrast, when we use a CSP, @italic{all we need are the rules}. The CSP solver does the rest. In this way, a CSP gives us an alternative, simpler way to explain Sudoku to the computer, just like regular expressions are an alternate way of expressing string patterns. And if the CSP solver is half a second slower, that seems like a reasonable tradeoff.

@margin-note{Daring minds might even consider a CSP solver to be a kind of domain-specific language.}

@section{Making & solving CSPs}


@defproc[(make-csp [vars (listof var?) null] 
                   [constraints (listof constraint?) empty])
         csp?]{
Create a new CSP. Variables and constraints can be added to the CSP by passing them as arguments. Or you can create an empty CSP and then add variables and constraints imperatively (e.g., with @racket[add-var!] or @racket[add-constraint!]).
}


@deftogether[(
@defproc[(add-var! 
[prob csp?] 
[name var-name?]
[domain (or/c (listof any/c) procedure?) empty])
void?]
@defproc[(add-vars! 
[prob csp?]
[names (listof var-name?)]
[domain (or/c (listof any/c) procedure?) empty])
void?]
)]{
Imperatively add a new variable called @racket[_name] to the CSP with permissible values listed in @racket[_domain]. The solution to a CSP is a list of pairs where each variable has been assigned a value from its domain.

@racket[add-vars!] is the same, but adds multiple variables that have the same domain.
}

@deftogether[(
@defproc[(add-constraint! 
[prob csp?] 
[func procedure?]
[names (listof var-name?)]
[func-name (or/c #false var-name?) #f])
void?]
@defproc[(add-constraints! 
[prob csp?]
[func procedure?]
[namess (listof (listof var-name?))]
[func-name (or/c #false var-name?) #f])
void?]
)]{
Imperatively add a new constraint. The constraint applies the function @racket[_func] to the list of variable names given in @racket[_names]. The return value of @racket[_func] does not need to be a Boolean, but any return value other than @racket[#false] is treated as if it were @racket[#true].

@racket[add-constraints!] is the same, but adds the constraint @racket[_func] to each list of variable names in @racket[_namess] (which is therefore a list of lists of variable names).
}

@defproc[(add-all-diff-constraint! 
[prob csp?] 
[names (listof var-name?) (map var-name (csp-vars prob))]
[#:same equal-proc equal?])
void?]{
Imperatively add an ``all diff'' constraint, which is a pairwise @racket[(compose1 not equal?)] constraint. A equality function other than @racket[equal?] can be passed via the @racket[#:same] argument. There is nothing special about using this function vs. applying the constraint manually. 
}


@defproc[(add-pairwise-constraint! 
[prob csp?] 
[func procedure?]
[names (listof var-name?)]
[func-name (or/c #false var-name?) #f])
void?]{
Similar to @racket[add-constraint!], but it takes a two-arity procedure @racket[_func] and adds it as a constraint between each pair of names in @racket[_names].

Why? CSPs are more efficient with lower-arity constraints (roughly, because you can rule out invalid values sooner). So usually, decomposing a larger-arity constraint into a group of smaller ones is a good idea. 

For instance, suppose you have three variables, and you want them to end up holding values that are coprime. Your constraint function is @racket[coprime?]. This function is variadic (meaning, it can take any number of arguments) so you could use @racket[add-constraint!] like so:

@racketblock[
(add-constraint! my-csp coprime? '(a b c))
]

But because the comparison can be done two at a time, we could write this instead:

@racketblock[
(add-pairwise-constraint! my-csp coprime? '(a b c))
]

Which would be equivalent to:

@racketblock[
(add-constraint! my-csp coprime? '(a b))
(add-constraint! my-csp coprime? '(b c))
(add-constraint! my-csp coprime? '(a c))
]

Still, @racket[add-pairwise-constraint!] doesn't substitute for thoughtful constraint design. For instance, suppose instead we want our variables to be strictly increasing. This time, our constraint function is @racket[<]:

@racketblock[
(add-constraint! my-csp < '(a b c))
]

And we could instead write:

@racketblock[
(add-pairwise-constraint! my-csp < '(a b c))
]

Which would become:

@racketblock[
(add-constraint! my-csp < '(a b))
(add-constraint! my-csp < '(b c))
(add-constraint! my-csp < '(a c))
]

This is better, but also overkill, because if @racket[(< a b)] and @racket[(< b c)], then by transitivity, @racket[(< a c)] is necessarily true. So this is a case where pairwise expands into more constraints than we actually need. This will not produce any wrong solutions, but especially on larger lists of variables, it creates unnecessary work that my slow down the solution search.
}


@defproc[(add-transitive-constraint! 
[prob csp?] 
[func procedure?]
[names (listof var-name?)]
[func-name (or/c #false var-name?) #f])
void?]{
Similar to @racket[add-pairwise-constraint!], but adds the constraint between every @italic{sequential} pair of names in @racket[_names] (not every @italic{possible} pair).

For instance, consider this use of @racket[add-pairwise-constraint!]:

@racketblock[
(add-pairwise-constraint! my-csp < '(a b c d))
]

This applies the constraint between every possible pair, so the result is equivalent to:

@racketblock[
(add-constraint! my-csp < '(a b))
(add-constraint! my-csp < '(a c))
(add-constraint! my-csp < '(a d))
(add-constraint! my-csp < '(b c))
(add-constraint! my-csp < '(b d))
(add-constraint! my-csp < '(c d))
]

This isn't wrong, but as any seventh grader could tell you, it's overkill. @racket[<] is a transitive relation, therefore if it's true that @racket[(< a b)] and @racket[(< b c)], it's necessarily also true that @racket[(< a c)]. So there's no need to apply a separate constraint for that.

This is the behavior we get from @racket[add-transitive-constraint!]. For instance if we instead write this:

@racketblock[
(add-transitive-constraint! my-csp < '(a b c d))
]

The constraint is applied between every sequential pair, so the result is equivalent to:

@racketblock[
(add-constraint! my-csp < '(a b))
(add-constraint! my-csp < '(b c))
(add-constraint! my-csp < '(c d))
]

Same truth in half the constraints.
}


@defproc[(make-var-names 
[prefix string?]
[vals (listof any/c)]
[suffix string? ""])
(listof symbol?)]{
Helper function to generate mass quantities of variable names. The @racket[_prefix] and (optional) @racket[_suffix] strings are wrapped around each value in @racket[_vals], and converted to a symbol.

@my-examples[
(make-var-names "foo" (range 6) "bar")
(make-var-names "col" (range 10))
]
}


@defproc[(solve 
[prob csp?] )
(or/c #false (listof (cons/c symbol? any/c)))]{
Return a solution for the CSP, or @racket[#false] if no solution exists.
}

@defproc[(solve* 
[prob csp?] 
[count natural? +inf.0])
(listof (listof (cons/c symbol? any/c)))]{
Return all the solutions for the CSP. If there are none, returns @racket[null]. The optional @racket[_count] argument returns a certain number of solutions (or fewer, if not that many solutions exist)
}

@defform[(in-solutions prob count)]{
Iterator form for use with @racket[for] loops that incrementally returns solutions to @racket[_prob], up to a maximum of @racket[_count].
}


@section{Sideshows}

@defproc[(state-count
[prob csp?])
natural?]{
Number of possible variable assignments for @racket[_prob], otherwise known as the state space. This is the product of the domain sizes of each variable. So a CSP that assigns five variables, each of which can have the values @racket["a-z"], has a state count of @racket[(expt 5 26)] = @racket[1490116119384765625]. 
}

@defproc[(csp->graph
[prob csp?])
graph?]{
Create an undirected graph (using Racket's @racketmodname[graph] library) where each CSP variable is represented in the graph as a vertex, and each constraint between any pair of variables is represented as an edge.
}

@defproc[(csp->graphviz
[prob csp?])
string?]{
Produce a Graphviz representation of the CSP that can be rendered into a beautiful diagram.
}

@section{Parameters}

@defparam[current-select-variable val (or/c #false procedure?) #:value #f]{
Next variable that the CSP solver will attempt to assign a value to. If @racket[#false], solver just picks the first unassigned variable.
}

@defparam[current-order-values val (or/c #false procedure?) #:value #f]{
Procedure that orders the remaining values in a domain. Default is @racket[#false], which means that the domain values are tried in their original order. If bad values are likely to be clustered together, it can be worth trying @racket[shuffle] for this parameter, which randomizes which value gets chosen next. Shuffling is also helpful in CSPs where all the variable values must be different (because otherwise, the values for every variable are tried in the same order, which means that the search space is front-loaded with failure).
}

@defparam[current-inference val (or/c #false procedure?) #:value forward-check]{
Current inference rule used by the solver. If @racket[#false], solver uses @racket[no-inference]. Default is @racket[forward-check].
}

@defparam[current-solver val procedure? #:value backtracking-solver]{
Current solver algorithm used to solve the CSP. Default is @racket[backtracking-solver].
}

@defparam[current-decompose val (or/c #false procedure?) #:value #t]{
Whether the CSP will be decomposed into independent subproblems (if possible), because smaller CSPs are typically easier to solve than larger ones (and then the component solutions are reassembled into a larger solution).
}

@defparam[current-thread-count val (or/c #false natural?) #:value 4]{
Number of threads used by the @racket[min-conflicts-solver].
}

@defparam[current-node-consistency val (or/c #false procedure?) #:value #f]{
Whether node consistency is applied. Node consistency is helpful for certain CSPs, but not others, so it is @racket[#false] by default.

Helpful for which CSPs? @italic{Node consistency} means that for any one-arity (aka unary) constraints on a variable, we can filter out any domain values that don't satisfy the constraint, thereby reducing the size of the search space. So if the CSP starts with unary constraints, and the constraints foreclose certain values, node consistency can be useful. The cost of node consistency is proportional to the number of values in the domain (because all of them have to be tested).

Node consistency tends to be especially helpful in CSPs where all the assignment values have to be different, and even more so where the variables all have the same domain (say, 100 variables, each with a value between 0 and 99 inclusive). In a case like this, any assignment to one variable means that value can no longer be used by any other variable. Node consistency will remove these values from the other variable domains, thereby pruning the search space aggressively.
}

@defparam[current-arity-reduction val (or/c #false procedure?) #:value #t]{
Whether constraints are reduced in arity where possible. This usually helps, so the default is @racket[#true].

Why does it help? Because lower-arity constraints tend to be faster to test, and the solver can use node consistency on one-arity constraints (see @racket[current-node-consistency]). 

For instance, suppose we have variables representing positive integers @racket[a] and @racket[b] and the constraint says @racket[(< a b)]. Further suppose that @racket[b] is assigned value @racket[5]. At that point, this constraint can be expressed instead as the one-arity function @racket[(< a 5)]. This implies that there are only four possible values for @racket[a] (namely, @racket['(1 2 3 4)])). If node consistency is active, the domain of @racket[a] can immediately be checked to see if it includes any of those values. But none of this is possible if we don't reduce the arity.
}

@section{Solvers}

Pass these functions to @racket[current-solver].

@defproc[(backtracking-solver
[prob csp?])
generator?]{
The default solver. Conducts an exhaustive, deterministic search of the state space. @italic{Backtracking} means that when the solver reaches a dead end in the search space, it unwinds to the last successful variable assignment and tries again. The details of its behavior are modified by @racket[current-select-variable], @racket[current-inference], and @racket[current-node-consistency].

The advantage of the backtracking solver: it proceeds through the search space in a systematic matter. If there is a solution, the backtracking solver will find it. Eventually. 

The disadvantage: the same. Some search spaces are so huge, and the solutions so rare, that concentrating the effort on searching any particular branch is likely to be futile. For a more probabilistic approach, try @racket[min-conflicts-solver].
}

@defproc[(min-conflicts-solver
[prob csp?]
[max-steps exact-positive-integer? 100])
generator?]{
An alternative solver. Begins with a random assignment and then tries to minimize the number of conflicts (that is, constraint violations), up to @racket[_max-steps] (which defaults to 100). In essence, this is a probabilistic hill-climbing algorithm, where the solver makes random guesses and then tries to nudge those guesses toward the correct answer. 

I like to imagine the solver flying above the search space with a planeload of paratroopers, who are dropped into the search territory. Each of them tries to walk from the place they land (= the initial random assignment) toward a solution. 

It's a little weird that this works at all, but it does. Sometimes even better than the @racket[backtracking-solver], because the minimum-conflicts solver is ``sampling'' the search space at many diverse locations. Whereas the @racket[backtracking-solver] can get stuck in a fruitless area of the search space, the minimum-conflicts solver keeps moving around.

Of course, to avoid getting stuck, the minimum-conflicts solver has to abandon guesses that aren't panning out. Hence the @racket[_max-steps] argument, which controls the number of steps the solver takes on a certain attempt before giving up.

The other parameter that affects this solver is @racket[current-thread-count], which defaults to 4. The solver is multithreaded in the sense that it pursues multiple solutions simultaneously. This way, if one thread finds a solution earlier, it will not be blocked by the others.
}


@section{Selecting the next variable}

Pass these functions to @racket[current-select-variable].

@defproc[(mrv-degree-hybrid
[prob csp?])
(or/c #false var?)]{
Selects next variable for assignment by choosing the one with the fewest values in its domain (aka @italic{minimum remaining values} or @italic{mrv}; see also @racket[minimum-remaining-values]) and largest number of constraints (aka @italic{degree}; see also @racket[max-degree]). The idea is that this variable is likely to fail more quickly than others, so we'd rather trigger that failure as soon as we can (in which case we know we need to explore a different part of the state space).
}

@defproc[(minimum-remaining-values
[prob csp?])
(or/c #false var?)]{
Selects next variable for assignment by choosing the one with the fewest values in its domain.
}

@defproc[(max-degree
[prob csp?])
(or/c #false var?)]{
Selects next variable for assignment by choosing the one with the largest number of constraints.
}

@section{Inference}

Pass these functions to @racket[current-inference].

@defproc[(forward-check
[prob csp?]
[name var-name?])
csp?]{
Used for inference when @racket[current-inference] is not otherwise set. Forward checking determines whether the assignment to @racket[_name] necessarily causes another variable domain to become empty. How? It examines the remaining two-arity constraints that link variable @racket[_name] to an unassigned variable. For each of these constraints, it plugs in the new value for @racket[_name] and checks that the other variable still has values in its domain that can meet the constraint. If not, the assignment to @racket[_name] must fail. Forward checking can discover failures faster than backtracking alone.
}

@defproc[(ac-3
[prob csp?]
[name var-name?])
csp?]{
Applies the AC-3 arc-consistency algorithm. Similar to forward checking, but checks farther ahead. For that reason, it will usually take longer. (It is not necessarily better, however.)

Specifically: following a new variable assignment, AC-3 examines all constraints that link exactly two unassigned variables. It checks that each variable has at least one value in its domain that can be paired with the other to satisfy the constraint (this pair comprises the eponymous @italic{arc}). If no such pair exists, then the constraint can never be satisfied, so the new variable assignment must fail. 

``So AC-3 is a superset of @racket[forward-check]?" Yes. Both techniques examine two-arity constraints after variable @racket[_name] has been assigned a value. Forward checking, however, only examines two-arity functions that include variable @racket[_name] in the constraint. Whereas AC-3 checks @italic{all} two-arity functions (even those that don't include @racket[_name]). 

In this way, AC-3 can detect inconsistencies that forward checking would miss. For instance, consider a CSP with three variables @italic{a} @italic{b} and @italic{c}, and three constraints @italic{ab}, @italic{ac}, and @italic{ab}. We assign a value to @italic{a}. Forward checking would then check constraints @italic{ab} and @italic{ac}, perhaps removing values from the domains of @italic{b} and @italic{c} to be consistent with the new value of @italic{a}. These domain reductions, however, might be inconsistent with constraint @italic{bc}. Forward checking won't notice this, because it never tests @italic{bc}. But AC-3 does test @italic{bc}, so it would notice the inconsistency.

The problem with AC-3 is that it's necessarily recursive: each time it eliminates a domain value from a certain variable, it has to recheck all the two-arity constraints (because any of them might have been made inconsistent by the removal of this value). AC-3 only stops when it can no longer remove any value from any domain. So yes, compared to simple forward checking, it does more. But it also potentially costs a lot more, especially if the variables have large domains.
}


@defproc[(no-inference
[prob csp?]
[name var-name?])
csp?]{
Truth in advertising: performs no inference.
}



@section{Structure types & predicates}


@defstruct[csp ([vars (listof var?)]
                 [constraints (listof constraint?)])
           #:transparent]{
Represents a CSP.
}

@defstruct[var ([name var-name?]
                 [domain (listof any/c)])
           #:transparent]{
Represents a variable in a CSP.
}

@defstruct[constraint ([names (listof var-name?)]
                 [proc procedure?])
           #:transparent]{
Represents a constraing in a CSP.
}

@defproc[(var-name?
[x any/c])
boolean?]{
Check whether @racket[_x] is a valid CSP variable name, which today can mean any value, but I might change my mind.
}

@section{License & source code}

This module is licensed under the MIT license.

Source repository at @link["http://github.com/mbutterick/csp"]{http://github.com/mbutterick/csp}. Suggestions & corrections welcome.

