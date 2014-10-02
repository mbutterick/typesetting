#!/usr/bin/python

from constraint import *

#p = Problem()
#p.addVariable("ab", [1, 2])
#p.addVariable("c", [3])
#print p.getSolutions()

problem = Problem()
problem.addVariables(["a", "b"], [1, 2])
problem.addConstraint(AllDifferentConstraint())
print problem.getSolutions()