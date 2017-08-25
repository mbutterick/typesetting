#lang scribble/manual

@(require scribble/eval (for-label racket xenomorph))

@(define my-eval (make-base-eval))
@(my-eval `(require xenomorph))


@title{Xenomorph: binary decoding & encoding}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodule[xenomorph]

Hands up: who likes reading and writing binary structures? 

OK, great. All of you are free to go. 

Everyone else: keep reading. This library is for you.

@italic{Derived principally from Devon Govett's @link["https://github.com/devongovett/restructure"]{@tt{restructure}} library for Node.}


@section{Installation}

At the command line:
@verbatim{raco pkg install xenomorph}

After that, you can update the package from the command line:
@verbatim{raco pkg update xenomorph}

Invoke the library in a source file by importing it in the usual way: @code{(require xenomorph)}. 

@section{Quick demo}


@section{The big idea}

@section{Numbers}



@section{License & source code}

This module is licensed under the LGPL.


Source repository at @link["http://github.com/mbutterick/xenomorph"]{http://github.com/mbutterick/xenomorph}. Suggestions & corrections welcome.

