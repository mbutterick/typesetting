#!/usr/bin/python
#
# ALBERT EINSTEIN'S RIDDLE 
#  
# ARE YOU IN THE TOP 2% OF INTELLIGENT PEOPLE IN THE WORLD? 
# SOLVE THE RIDDLE AND FIND OUT. 
# 
# There are no tricks, just pure logic, so good luck and don't give up. 
# 
# 1. In a street there are five houses, painted five different colours. 
# 2. In each house lives a person of different nationality 
# 3. These five homeowners each drink a different kind of beverage, smoke 
# different brand of cigar and keep a different pet. 
# 
# THE QUESTION: WHO OWNS THE zebra? 
# 
# HINTS 
# 
# 1. The englishman lives in a red house. 
# 2. The spaniard keeps dogs as pets. 
# 5. The owner of the Green house drinks coffee. 
# 3. The ukrainian drinks tea. 
# 4. The Green house is on the left of the ivory house. 
# 6. The person who smokes oldgold rears snails. 
# 7. The owner of the Yellow house smokes kools. 
# 8. The man living in the centre house drinks milk. 
# 9. The Norwegian lives in the first house. 
# 10. The man who smokes chesterfields lives next to the one who keeps foxes. 
# 11. The man who keeps horses lives next to the man who smokes kools. 
# 12. The man who smokes luckystrike drinks orangejuice. 
# 13. The japanese smokes parliaments. 
# 14. The Norwegian lives next to the blue house. 
# 15. The man who smokes chesterfields has a neighbour who drinks water. 
# 
# ALBERT EINSTEIN WROTE THIS RIDDLE EARLY DURING THE 19th CENTURY. HE 
# SAID THAT 98% OF THE WORLD POPULATION WOULD NOT BE ABLE TO SOLVE IT. 

from constraint import *

# Check http://www.csc.fi/oppaat/f95/python/talot.py

def main():
    problem = Problem()
    for i in range(1,6):
        problem.addVariable("color%d" % i, 
                            ["red", "ivory", "green", "yellow", "blue"])
        problem.addVariable("nationality%d" % i,
                            ["englishman", "spaniard", "ukrainian", "norwegian", "japanese"])
        problem.addVariable("drink%d" % i,
                            ["tea", "coffee", "milk", "orangejuice", "water"])
        problem.addVariable("smoke%d" % i,
                            ["oldgold", "kools", "chesterfields",
                             "luckystrike", "parliaments"])
        problem.addVariable("pet%d" % i,
                            ["dogs", "snails", "foxes", "horses", "zebra"])

    problem.addConstraint(AllDifferentConstraint(),
                          ["color%d" % i for i in range(1,6)])
    problem.addConstraint(AllDifferentConstraint(),
                          ["nationality%d" % i for i in range(1,6)])
    problem.addConstraint(AllDifferentConstraint(),
                          ["drink%d" % i for i in range(1,6)])
    problem.addConstraint(AllDifferentConstraint(),
                          ["smoke%d" % i for i in range(1,6)])
    problem.addConstraint(AllDifferentConstraint(),
                          ["pet%d" % i for i in range(1,6)])

    for i in range(1,6):

        # Hint 1
        problem.addConstraint(lambda nationality, color:
                              nationality != "englishman" or color == "red",
                              ("nationality%d" % i, "color%d" % i))

        # Hint 2
        problem.addConstraint(lambda nationality, pet:
                              nationality != "spaniard" or pet == "dogs",
                              ("nationality%d" % i, "pet%d" % i))

        # Hint 3
        problem.addConstraint(lambda nationality, drink:
                              nationality != "ukrainian" or drink == "tea",
                              ("nationality%d" % i, "drink%d" % i))

        # Hint 4
        if i < 5:
            problem.addConstraint(lambda colora, colorb:
                                  colora != "green" or colorb == "ivory",
                                  ("color%d" % i, "color%d" % (i+1)))
        else:
            problem.addConstraint(lambda color: color != "green",
                                  ("color%d" % i,))

        # Hint 5
        problem.addConstraint(lambda color, drink:
                              color != "green" or drink == "coffee",
                              ("color%d" % i, "drink%d" % i))

        # Hint 6
        problem.addConstraint(lambda smoke, pet:
                              smoke != "oldgold" or pet == "snails",
                              ("smoke%d" % i, "pet%d" % i))

        # Hint 7
        problem.addConstraint(lambda color, smoke:
                              color != "yellow" or smoke == "kools",
                              ("color%d" % i, "smoke%d" % i))

        # Hint 8
        if i == 3:
            problem.addConstraint(lambda drink: drink == "milk",
                                  ("drink%d" % i,))

        # Hint 9
        if i == 1:
            problem.addConstraint(lambda nationality:
                                  nationality == "norwegian",
                                  ("nationality%d" % i,))

        # Hint 10
        if 1 < i < 5:
            problem.addConstraint(lambda smoke, peta, petb:
                                  smoke != "chesterfields" or peta == "foxes" or
                                                       petb == "foxes",
                                  ("smoke%d" % i, "pet%d" % (i-1),
                                                  "pet%d" % (i+1)))
        else:
            problem.addConstraint(lambda smoke, pet:
                                  smoke != "chesterfields" or pet == "foxes",
                                  ("smoke%d" % i,
                                   "pet%d" % (i == 1 and 2 or 4)))

        # Hint 11
        if 1 < i < 5:
            problem.addConstraint(lambda pet, smokea, smokeb:
                                  pet != "horses" or smokea == "kools" or
                                                     smokeb == "kools",
                                  ("pet%d" % i, "smoke%d" % (i-1),
                                                "smoke%d" % (i+1)))
        else:
            problem.addConstraint(lambda pet, smoke:
                                  pet != "horses" or smoke == "kools",
                                  ("pet%d" % i,
                                   "smoke%d" % (i == 1 and 2 or 4)))
        
        # Hint 12
        problem.addConstraint(lambda smoke, drink:
                              smoke != "luckystrike" or drink == "orangejuice",
                              ("smoke%d" % i, "drink%d" % i))

        # Hint 13
        problem.addConstraint(lambda nationality, smoke:
                              nationality != "japanese" or smoke == "parliaments",
                              ("nationality%d" % i, "smoke%d" % i))

        # Hint 14
        if 1 < i < 5:
            problem.addConstraint(lambda nationality, colora, colorb:
                                  nationality != "norwegian" or
                                  colora == "blue" or colorb == "blue",
                                  ("nationality%d" % i, "color%d" % (i-1),
                                                        "color%d" % (i+1)))
        else:
            problem.addConstraint(lambda nationality, color:
                                  nationality != "norwegian" or
                                  color == "blue",
                                  ("nationality%d" % i,
                                   "color%d" % (i == 1 and 2 or 4)))



    solutions = problem.getSolutions()
    print "Found %d solution(s)!" % len(solutions)
    print
    for solution in solutions:
        showSolution(solution)

def showSolution(solution):
    for i in range(1,6):
        print "House %d" % i
        print "--------"
        print "Nationality: %s" % solution["nationality%d" % i]
        print "Color: %s" % solution["color%d" % i]
        print "Drink: %s" % solution["drink%d" % i]
        print "Smoke: %s" % solution["smoke%d" % i]
        print "Pet: %s" % solution["pet%d" % i]
        print
                                  
if __name__ == "__main__":
    main()
