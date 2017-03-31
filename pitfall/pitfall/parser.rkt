#lang brag

pf-program : [pf-header] pf-object*
pf-header : PDF-HEADER
@pf-object : pf-null | CHAR | BOOLEAN | INT | REAL | pf-name | pf-string | pf-array | pf-dict | pf-stream | pf-indirect-object | pf-indirect-object-ref | pf-comment
@pf-null : NULL
pf-name : NAME
pf-string : STRING-TOK | /"<" HEX-DIGIT-PAIR* /">"
pf-array : /"[" pf-object* /"]"
pf-dict : /"<" /"<" (pf-dict-key pf-dict-value)* /">" /">"
@pf-dict-key : pf-object
@pf-dict-value : pf-object
pf-stream : pf-dict STREAM-DATA
pf-indirect-object : INT INT /"obj" pf-object /"endobj"
pf-indirect-object-ref : INDIRECT-OBJECT-REF-TOK
pf-comment : COMMENT