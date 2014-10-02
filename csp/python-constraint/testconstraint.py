#!/usr/bin/python

from constraint import *

p = Problem()
p.addVariable("ab", [1, 2])
p.addVariable("c", [3])
print p.getSolutions()