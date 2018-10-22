#lang scribble/manual

@(require scribble/eval (for-label racket csp))

@(define my-eval (make-base-eval))
@(my-eval `(require csp))


@title{csp}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]


@defmodule[csp]

A simple hyphenation engine that uses the Knuth–Liang hyphenation algorithm originally developed for TeX. I have added little to their work. Accordingly, I take little credit.

@section{Installation}

At the command line:
@verbatim{raco pkg install csp}

After that, you can update the package like so:
@verbatim{raco pkg update csp}

@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/csp"]{http://github.com/mbutterick/csp}. Suggestions & corrections welcome.

