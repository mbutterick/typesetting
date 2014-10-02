#!/usr/bin/python

from constraint import *

#p = Problem()
#p.addVariable("ab", [1, 2])
#p.addVariable("c", [3])
#print p.getSolutions()

problem = Problem()
problem.addVariables(["a", "b"], [1, 2])
def func(a, b):
    return b > a
problem.addConstraint(func, ["a", "b"])
problem.getSolution()
