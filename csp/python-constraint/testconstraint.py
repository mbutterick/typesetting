#!/usr/bin/python

from constraint import *

p = Problem()
p.addVariable("a", [1])
print p.getSolutions()