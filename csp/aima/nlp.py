"""A chart parser and some grammars. (Chapter 22)"""

from utils import *

#______________________________________________________________________________
# Grammars and Lexicons

def Rules(**rules): 
    """Create a dictionary mapping symbols to alternative sequences.
    >>> Rules(A = "B C | D E")
    {'A': [['B', 'C'], ['D', 'E']]}
    """
    for (lhs, rhs) in rules.items():
        rules[lhs] = [alt.strip().split() for alt in rhs.split('|')]
    return rules

def Lexicon(**rules):
    """Create a dictionary mapping symbols to alternative words.
    >>> Lexicon(Art = "the | a | an")
    {'Art': ['the', 'a', 'an']}
    """    
    for (lhs, rhs) in rules.items():
        rules[lhs] = [word.strip() for word in rhs.split('|')]
    return rules

class Grammar:
    def __init__(self, name, rules, lexicon):
        "A grammar has a set of rules and a lexicon."
        update(self, name=name, rules=rules, lexicon=lexicon)
        self.categories = DefaultDict([])
        for lhs in lexicon:
            for word in lexicon[lhs]:
                self.categories[word].append(lhs)

    def rewrites_for(self, cat):
        "Return a sequence of possible rhs's that cat can be rewritten as."
        return self.rules.get(cat, ())

    def isa(self, word, cat):
        "Return True iff word is of category cat"
        return cat in self.categories[word]

    def __repr__(self):
        return '<Grammar %s>' % self.name
    
E0 = Grammar('E0',
    Rules( # Grammar for E_0 [Fig. 22.4]
    S = 'NP VP | S Conjunction S',
    NP = 'Pronoun | Noun | Article Noun | Digit Digit | NP PP | NP RelClause',
    VP = 'Verb | VP NP | VP Adjective | VP PP | VP Adverb',
    PP = 'Preposition NP',
    RelClause = 'That VP'),

    Lexicon( # Lexicon for E_0 [Fig. 22.3]
    Noun = "stench | breeze | glitter | nothing | wumpus | pit | pits | gold | east",
    Verb = "is | see | smell | shoot | fell | stinks | go | grab | carry | kill | turn | feel",
    Adjective = "right | left | east | south | back | smelly",
    Adverb = "here | there | nearby | ahead | right | left | east | south | back",
    Pronoun = "me | you | I | it",
    Name = "John | Mary | Boston | Aristotle",
    Article = "the | a | an",
    Preposition = "to | in | on | near",
    Conjunction = "and | or | but",
    Digit = "0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9",
    That = "that"
    ))

E_ = Grammar('E_', # Trivial Grammar and lexicon for testing
    Rules(
    S = 'NP VP',
    NP = 'Art N | Pronoun',
    VP = 'V NP'),

    Lexicon(
    Art = 'the | a',
    N = 'man | woman | table | shoelace | saw',
    Pronoun = 'I | you | it',
    V = 'saw | liked | feel'
    ))

def generate_random(grammar=E_, s='S'):
    """Replace each token in s by a random entry in grammar (recursively).
    This is useful for testing a grammar, e.g. generate_random(E_)"""
    import random
    
    def rewrite(tokens, into):
        for token in tokens:
            if token in grammar.rules:
                rewrite(random.choice(grammar.rules[token]), into)
            elif token in grammar.lexicon:
                into.append(random.choice(grammar.lexicon[token]))
            else:
                into.append(token)
        return into

    return ' '.join(rewrite(s.split(), []))

#______________________________________________________________________________
# Chart Parsing


class Chart:
    """Class for parsing sentences using a chart data structure. [Fig 22.7]
    >>> chart = Chart(E0); 
    >>> len(chart.parses('the stench is in 2 2'))
    1
    """

    def __init__(self, grammar, trace=False):
        """A datastructure for parsing a string; and methods to do the parse.
        self.chart[i] holds the edges that end just before the i'th word.
        Edges are 5-element lists of [start, end, lhs, [found], [expects]]."""
        update(self, grammar=grammar, trace=trace)

    def parses(self, words, S='S'):
        """Return a list of parses; words can be a list or string."""
        if isinstance(words, str):
            words = words.split()
        self.parse(words, S)
        # Return all the parses that span the whole input
        return [[i, j, S, found, []]
                for (i, j, lhs, found, expects) in self.chart[len(words)]
                if lhs == S and expects == []]

    def parse(self, words, S='S'):
        """Parse a list of words; according to the grammar.
        Leave results in the chart."""
        self.chart = [[] for i in range(len(words)+1)]
        self.add_edge([0, 0, 'S_', [], [S]])
        for i in range(len(words)):
            self.scanner(i, words[i])
        return self.chart

    def add_edge(self, edge):
        "Add edge to chart, and see if it extends or predicts another edge."
        start, end, lhs, found, expects = edge
        if edge not in self.chart[end]:
            self.chart[end].append(edge)
            if self.trace:
                print '%10s: added %s' % (caller(2), edge)
            if not expects:
                self.extender(edge)
            else:
                self.predictor(edge)

    def scanner(self, j, word):
        "For each edge expecting a word of this category here, extend the edge."
        for (i, j, A, alpha, Bb) in self.chart[j]:
            if Bb and self.grammar.isa(word, Bb[0]):
                self.add_edge([i, j+1, A, alpha + [(Bb[0], word)], Bb[1:]])

    def predictor(self, (i, j, A, alpha, Bb)):
        "Add to chart any rules for B that could help extend this edge."
        B = Bb[0]
        if B in self.grammar.rules:
            for rhs in self.grammar.rewrites_for(B):
                self.add_edge([j, j, B, [], rhs])

    def extender(self, edge):
        "See what edges can be extended by this edge."
        (j, k, B, _, _) = edge
        for (i, j, A, alpha, B1b) in self.chart[j]:
            if B1b and B == B1b[0]:
                self.add_edge([i, k, A, alpha + [edge], B1b[1:]])


            
#### TODO: 
#### 1. Parsing with augmentations -- requires unification, etc.
#### 2. Sequitor
