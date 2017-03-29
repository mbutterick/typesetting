#lang brag

pf-program : pf-thing*
@pf-thing : pf-null | CHAR | BOOLEAN | INT | REAL | pf-name | pf-string | pf-array | pf-dict | pf-stream | pf-indirect-object | pf-indirect-object-ref
@pf-null : NULL
pf-name : NAME
pf-string : STRING-TOK | /"<" HEX-DIGIT-PAIR+ /">"
pf-array : /"[" pf-thing* /"]"
pf-dict : /"<<" (pf-dict-key pf-dict-value)* /">>"
@pf-dict-key : pf-thing
@pf-dict-value : pf-thing
pf-stream : pf-dict STREAM-DATA
pf-indirect-object : INT INT /"obj" pf-thing /"endobj"
pf-indirect-object-ref : INDIRECT-OBJECT-REF-TOK