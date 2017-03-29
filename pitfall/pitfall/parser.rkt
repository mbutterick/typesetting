#lang brag

pf-program : pf-thing*
@pf-thing : NULL | CHAR | BOOLEAN | INT | REAL | pf-name | pf-string | pf-array | pf-dict
pf-name : NAME
pf-string : PAREN-TOK | /LEFT-ANGLE HEX-DIGIT-PAIR+ /RIGHT-ANGLE
pf-array : /LEFT-BRACKET pf-thing* /RIGHT-BRACKET
pf-dict : /DOUBLE-LEFT-ANGLE (pf-dict-key pf-dict-value)* /DOUBLE-RIGHT-ANGLE
@pf-dict-key : pf-thing
@pf-dict-value : pf-thing