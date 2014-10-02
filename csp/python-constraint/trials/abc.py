#!/usr/bin/python
#
# What's the minimum value for:
#
#        ABC
#      -------
#       A+B+C
#
# From http://www.umassd.edu/mathcontest/abc.cfm
#
from constraint import *

def main():
    problem = Problem()
    problem.addVariables("abc", range(1,10))
    results = []
    for solution in problem.getSolutions():    
        a = solution["a"]
        b = solution["b"]
        c = solution["c"]
        results.append((((a*100) + (b*10) + c) / (a + b + c + 0.0), (a*100) + (b*10) + c))

    results.sort()

    print results[0]


if __name__ == "__main__":
    main()
