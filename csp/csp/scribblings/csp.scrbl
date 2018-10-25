#lang scribble/manual

@(require scribble/eval (for-label racket csp (except-in math/number-theory permutations)))

@(define my-eval (make-base-eval))
@(my-eval `(require csp racket/list))

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

A @deftech{constraint-satisfaction problem} (often shortened to @deftech{CSP}) has two ingredients. The first is a set of @deftech{variables}, each associated with a set of possible values (called a @deftech{domain}). The other is a set of @deftech{constraints} that define relationships between the variables. 

Solving a CSP means finding a value for each variable from its domain that @deftech{satisfies} (that is, doesn't violate) any constraints. This selection of values is also known as an @deftech{assignment}. A CSP may have any number of assignments that solve the problem (including zero). 

Even if the name is new, the idea of a CSP is probably familiar. For instance, many brain teasers — like Sudoku or crosswords or logic puzzles — are really just constraint-satisfaction problems. (Indeed, you can use this package to ruin all of them.) 

When the computer solves a CSP, it's using an analogous process of deductive reasoning to eliminate impossible assignments, eventually converging on a solution (or determining that no solution exists). 

@section{Example}

Suppose we wanted to find @link["http://www.friesian.com/pythag.htm"]{Pythagorean triples} with sides between 10 and 49, inclusive.

First we create a new CSP called @racket[triples], using @racket[make-csp]:

@examples[#:eval my-eval
(define triples (make-csp))
]


Then we need variables to represent the values in the triple. For that, we use @racket[add-var!], where each variable has a @tech{symbol} for its name and a list of values for its domain:

@examples[#:eval my-eval
(add-var! triples 'a (range 10 50))
(add-var! triples 'b (range 10 50))
(add-var! triples 'c (range 10 50))
]

Then we need our constraint. We make a function called @racket[valid-triple?] that tests three values to see if they qualify as a Pythagorean triple. Then we insert this function as a constraint using @racket[add-constraint!], where we pass in the function we want to use for the constraint, and a list of variable names that the constraint applies to. 

@examples[#:eval my-eval
(define (valid-triple? x y z)
  (= (expt z 2) (+ (expt x 2) (expt y 2))))

(add-constraint! triples valid-triple? '(a b c))
]

The argument names used within the constraint function have nothing to do with the CSP variable names that are passed to the function. This makes sense — we might want constraints that apply the same function to different groups of CSP variables. What's important is that the @tech{arity} of the constraint matches the number of variable names.

Finally we call @racket[solve], which finds a solution (if it exists):

@examples[#:eval my-eval
(solve triples)
]

``But that's just the 5--12--13 triple, doubled.'' True. If we wanted to ensure that the values in our solution have no common factors, we can add a new @racket[coprime?] constraint:

@examples[#:eval my-eval
(require math/number-theory)
(add-constraint! triples coprime? '(a b c))
]

And we can @racket[solve] again to see the new result:

@examples[#:eval my-eval
(solve triples)
]

Maybe we become curious to see how many of these triples exist. We can use @racket[solve*] to find all four solutions:

@examples[#:eval my-eval
(solve* triples)
]

``But really there's only two solutions — the values for @racket[a] and @racket[b] are swapped in the other two.'' Fair enough. We might say that this problem is @deftech{symmetric} relative to variables @racket[a] and @racket[b], because they have the same domains and are constrained the same way. We can break the symmetry by adding a constraint that forces @racket[a] to be less than or equal to @racket[b]:

@examples[#:eval my-eval
(add-constraint! triples <= '(a b))

(solve* triples)
]

Now our list of solutions doesn't have any symmetric duplicates. 

By the way, what if we had accidentally included @racket[c] in the last constraint? 

@examples[#:eval my-eval
(add-constraint! triples <= '(a b c))

(solve* triples)
]

Nothing changes. Why? Because @racket[c] is necessarily going to be larger because of the existing @racket[valid-triple?] constraint, so it always meets this constraint too. Still, it's good practice to minimize constraints — no need for the ``belt and suspenders'' approach.

We should use @racket[solve*] with care. It can't finish until the CSP solver examines every possible assignment of values in the problem, which can be a big number. Specifically, it's the product of the domain sizes of each variable, which in this case is 40 × 40 × 40 = 64,000. This realm of possible assignments is also known as the CSP's @deftech{state space}. We can also get this number from @racket[state-count]:

@examples[#:eval my-eval
(state-count triples)
]

It's easy for a CSP to have a state count in the zillions. For this reason we can supply @racket[solve*] with an optional @racket[#:limit] argument that will only generate a certain number of solutions: 

@examples[#:eval my-eval
(time (solve* triples))
(time (solve* triples #:limit 2))
]

Here, the answers are the same. But the second call to @racket[solve*] finishes sooner, because it quits as soon as it's found two solutions.

Of course, when we use ordinary @racket[solve], we don't know how many assignments it will have to try before it finds a solution. If the problem is impossible, even @racket[solve] will have to examine every possible assignment before it knows for sure. For instance, let's see what happens if we add a constraint that's impossible to meet:

@examples[#:eval my-eval
(add-constraint! triples = '(a b c))

(solve triples)
]

Disappointing but accurate.



@section{Making & solving CSPs}


@section{Sideshows}


@section{Parameters}



@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/csp"]{http://github.com/mbutterick/csp}. Suggestions & corrections welcome.

