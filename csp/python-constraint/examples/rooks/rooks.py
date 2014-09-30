#!/usr/bin/python
#
# http://mathworld.wolfram.com/RooksProblem.html
#
from constraint import *
import sys

def factorial(x): return x == 1 or factorial(x-1)*x

def main(show=False):
    problem = Problem()
    size = 8
    cols = range(size)
    rows = range(size)
    problem.addVariables(cols, rows)
    for col1 in cols:
        for col2 in cols:
            if col1 < col2:
                problem.addConstraint(lambda row1, row2: row1 != row2,
                                      (col1, col2))
    solutions = problem.getSolutions()
    print "Found %d solution(s)!" % len(solutions)
    assert len(solutions) == factorial(size)
    if show:
        for solution in solutions:
            showSolution(solution, size)

def showSolution(solution, size):
    sys.stdout.write("   %s \n" % ("-"*((size*4)-1)))
    for i in range(size):
        sys.stdout.write("  |")
        for j in range(size):
            if solution[j] == i:
                sys.stdout.write(" %d |" % j)
            else:
                sys.stdout.write("   |")
        sys.stdout.write("\n")
        if i != size-1:
            sys.stdout.write("  |%s|\n" % ("-"*((size*4)-1)))
    sys.stdout.write("   %s \n" % ("-"*((size*4)-1)))

if __name__ == "__main__":
    show = False
    if len(sys.argv) == 2 and sys.argv[1] == "-s":
        show = True
    elif len(sys.argv) != 1:
        sys.exit("Usage: rooks.py [-s]")
    main(show)

