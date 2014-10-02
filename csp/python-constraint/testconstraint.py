#!/usr/bin/python

from constraint import *

#p = Problem()
#p.addVariable("ab", [1, 2])
#p.addVariable("c", [3])
#print p.getSolutions()

problem = Problem()
problem.addVariables(["a", "b"], range(500))
def func(a, b):
    return a > 0 and b == 211 * a
problem.addConstraint(func, ["a", "b"])
print problem.getSolutions()
