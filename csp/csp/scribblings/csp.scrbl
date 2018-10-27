#lang scribble/manual

@(require (except-in scribble/eval examples) scribble/example (for-label racket csp graph (except-in math/number-theory permutations)))

@(define my-eval (make-base-eval))
@(my-eval `(require csp racket/list))

@(define-syntax-rule (my-examples ARG ...)
(examples #:label #f #:eval my-eval ARG ...))

@title{Constraint-satisfaction problems}

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

It's easy for a CSP to have a state count in the zillions. For this reason we can supply @racket[solve*] with an optional @racket[#:count] argument that will only generate a certain number of solutions: 

@examples[#:label #f #:eval my-eval
(time (solve* triples))
(time (solve* triples #:count 2))
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

(solve* triples #:count 2)
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

@defstruct[csp ([vars (listof var?)]
                 [constraints (listof constraint?)])
           #:transparent]{
TK
}

@defstruct[var ([name name?]
                 [domain (listof any/c)])
           #:transparent]{
TK
}

@defstruct[constraint ([names (listof name?)]
                 [proc procedure?])
           #:transparent]{
TK
}


@defproc[(make-csp [vars (listof var?) null] 
                   [constraints (listof constraint?) empty])
         csp?]{
TK
}


@deftogether[(
@defproc[(add-var! 
[prob csp?] 
[name name?]
[domain (or/c (listof any/c) procedure?) empty])
void?]
@defproc[(add-vars! 
[prob csp?]
[names (listof name?)]
[domain (or/c (listof any/c) procedure?) empty])
void?]
)]{
TK
}

@deftogether[(
@defproc[(add-constraint! 
[prob csp?] 
[func procedure?]
[names (listof name?)]
[func-name (or/c #false name?) #f])
void?]
@defproc[(add-constraints! 
[prob csp?]
[func procedure?]
[namess (listof (listof name?))]
[func-name (or/c #false name?) #f])
void?]
)]{
TK
}

@defproc[(add-pairwise-constraint! 
[prob csp?] 
[func procedure?]
[names (listof name?)]
[func-name (or/c #false name?) #f])
void?]{
TK
}

@defproc[(solve 
[prob csp?] 
[#:count count natural? 1])
(or/c #false any/c (listof any/c))]{
TK
}

@defproc[(solve* 
[prob csp?] 
[#:count count natural? +inf.0])
(listof any/c)]{
TK
}



@section{Sideshows}

@defproc[(state-count
[prob csp?])
natural?]{
TK
}

@defproc[(csp->graph
[prob csp?])
graph?]{
TK
}

@defproc[(csp->graphviz
[prob csp?])
string?]{
TK
}

@section{Parameters}


@defparam[current-select-variable val (or/c #false procedure?) #:value #f]{
TK
}

@defparam[current-order-values val (or/c #false procedure?) #:value #f]{
TK
}

@defparam[current-inference val (or/c #false procedure?) #:value #f]{
TK
}

@defparam[current-solver val (or/c #false procedure?) #:value #f]{
TK
}

@defparam[current-random val (or/c #false procedure?) #:value #t]{
TK
}

@defparam[current-decompose val (or/c #false procedure?) #:value #t]{
TK
}

@defparam[current-thread-count val (or/c #false natural?) #:value 4]{
TK
}

@defparam[current-node-consistency val (or/c #false procedure?) #:value #f]{
TK
}

@defparam[current-arity-reduction val (or/c #false procedure?) #:value #t]{
TK
}

@defparam[current-learning val (or/c #false procedure?) #:value #f]{
TK
}


@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/csp"]{http://github.com/mbutterick/csp}. Suggestions & corrections welcome.

