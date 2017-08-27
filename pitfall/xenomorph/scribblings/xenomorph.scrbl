#lang scribble/manual

@(require scribble/eval (for-label racket xenomorph))

@(define my-eval (make-base-eval))
@(my-eval `(require xenomorph))


@title{Xenomorph: binary decoding & encoding}

@author[(author+email "Matthew Butterick" "mb@mbtype.com") "Devon Govett"]

@defmodule[xenomorph]

Hands up: who likes parsing and writing binary structures? 

OK, just a few of you, in the back. You're free to go. 

Everyone else: Xenomorph eases the pain of working with binary formats. Instead of fiddling with counting bytes:

@itemlist[#:style 'ordered
@item{You build up a declaration of the binary format from its parts — like integers, strings, arrays, and pointers.}

@item{This declaration can then be used as a binary parser, converting a binary file to Racket values.}

@item{This same declaration can @emph{also} be used as a binary compiler, converting Racket values to a binary file.}
]

Derived principally from Devon Govett's @link["https://github.com/devongovett/restructure"]{@tt{restructure}} library for Node. Thanks for figuring out the hard parts, dude.


@section{Installation}

At the command line:
@verbatim{raco pkg install xenomorph}

After that, you can update the package from the command line:
@verbatim{raco pkg update xenomorph}

Invoke the library in a source file by importing it in the usual way: @code{(require xenomorph)}. 

@section{Quick start}

@examples[#:eval my-eval
(define four-ints (+ArrayT uint8 4))

(decode four-ints #"\1\2\3\4")
(decode four-ints #"\1\2\3")
(decode four-ints #"\1\2\3\4\5\6")

(define op (open-output-string))
(encode four-ints '(1 2 3 4) op)
(get-output-bytes op)
   ]



@section{The big idea}

@section{Numbers}



@section{License & source code}

This module is licensed under the LGPL.


Source repository at @link["http://github.com/mbutterick/xenomorph"]{http://github.com/mbutterick/xenomorph}. Suggestions & corrections welcome.

