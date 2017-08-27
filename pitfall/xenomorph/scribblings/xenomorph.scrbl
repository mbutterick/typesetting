#lang scribble/manual

@(require scribble/eval (for-label racket xenomorph))

@(define my-eval (make-base-eval))
@(my-eval `(require xenomorph))


@title{Xenomorph: binary decoding & encoding}

@author[(author+email "Matthew Butterick" "mb@mbtype.com") "Devon Govett"]

@defmodule[xenomorph]

Hands up: who likes parsing and writing binary formats?

OK, just a few of you, in the back. You're free to go. 

Everyone else: Xenomorph eases the pain of working with binary formats. Instead of fiddling with counting bytes:

@itemlist[#:style 'ordered
@item{You define a @deftech{template} describing the binary format using smaller ingredients — like integers, strings, arrays, pointers, and other templates.}

@item{This template can then be used as a binary parser, reading bytes and parsing them into Racket values. This function is called @racket[decode].}

@item{This @emph{same} template can also be used as a binary compiler, compiling Racket values to binary and writing them out to a file. This function is called @racket[encode].}
]

Derived principally from Devon Govett's @link["https://github.com/devongovett/restructure"]{@tt{restructure}} library for Node. Thanks for doing the heavy lifting, dude.


@section{Installation}

At the command line:

@verbatim{raco pkg install xenomorph}

After that, you can update the package from the command line:

@verbatim{raco pkg update xenomorph}

Invoke the library in a source file by importing it in the usual way:

@verbatim{(require xenomorph)}


@section{What is a binary format?}

Suppose we have a file on disk. What's in the file? Without knowing anything else, we can at least say the file contains a sequence of @deftech{bytes}. A @deftech{byte} is the smallest unit of data storage. Though not the smallest unit of information storage — that would be a @deftech{bit}. But when we read (or write) from disk (or other source), we work with bytes.

Commonly, files on disk are classified as being either in @deftech{binary} format or @emph{text} format. (A distinction observed by Racket functions such as @racket[write-to-file].) When we speak of binary vs. text, we are saying something about the @deftech{encoding} of the file: what do the bytes represent?

A text format is really just a special kind of binary format where the result is a human readable text string. Moreover, for ease simplicity, text files typically rely on pre-defined encodings.

But this is a custom, not a requirement. 



@section{Quick tutorial}

@examples[#:eval my-eval
(define four-ints (+ArrayT uint8 4))

(decode four-ints #"\1\2\3\4")
(decode four-ints #"\1\2\3")
(decode four-ints #"\1\2\3\4\5\6")

(define op (open-output-string))
(encode four-ints '(1 2 3 4) op)
(get-output-bytes op)
   ]


@section{More involved tutorial}


@section{Core functions}

@defproc[
(decode
[template (is-a?/c xenomorph-base%)]
[byte-source (or/c bytes? input-port?) (current-input-port)])
any/c]{
Hello
}

@defproc[
(encode
[template (is-a?/c xenomorph-base%)]
[v any/c]
[byte-dest (or/c output-port? #f) (current-output-port)])
(or/c void? bytes?)]{
Hello
}

@defproc[
(size
[template (is-a?/c xenomorph-base%)]
[v any/c])
exact-nonnegative-integer?]{
Hello
}


@section{Binary ingredients}



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

This module is licensed under the MIT license.


Source repository at @link["http://github.com/mbutterick/xenomorph"]{http://github.com/mbutterick/xenomorph}. Suggestions & corrections welcome.

