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

@item{This @emph{same} declaration can also be used as a binary compiler, converting Racket values to a binary file.}
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


@section{Binary parts}

@subsection{Numbers}

@defmodule[xenomorph/number]

@subsection{Strings}

@defmodule[xenomorph/string]

@subsection{Arrays}

@defmodule[xenomorph/array]

@subsection{Lazy arrays}

@defmodule[xenomorph/lazy-array]


@subsection{Structs}

@defmodule[xenomorph/struct]

@subsection{Versioned structs}

@defmodule[xenomorph/versioned-struct]

@subsection{Pointers}

@defmodule[xenomorph/pointer]

@subsection{Bitfields}

@defmodule[xenomorph/bitfield]

@subsection{Enumerations}

@defmodule[xenomorph/enum]

@subsection{Optional}

@defmodule[xenomorph/optional]

@subsection{Reserved}

@defmodule[xenomorph/reserved]



@defproc[
(array?
[type any/c])
void?]{
Hello
}


@section{License & source code}

This module is licensed under the LGPL.


Source repository at @link["http://github.com/mbutterick/xenomorph"]{http://github.com/mbutterick/xenomorph}. Suggestions & corrections welcome.

