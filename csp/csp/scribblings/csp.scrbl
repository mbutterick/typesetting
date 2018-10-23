#lang scribble/manual

@(require scribble/eval (for-label racket csp))

@(define my-eval (make-base-eval))
@(my-eval `(require csp))

@title{Constraint-satisfaction problems}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodule[csp]

@margin-note{This package is in development. I make no commitment to maintaining the public interface documented below.}

A simple solver for constraint-satisfaction problems.

@section{Installation}

At the command line:
@verbatim{raco pkg install csp}

After that, you can update the package like so:
@verbatim{raco pkg update csp}

@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/csp"]{http://github.com/mbutterick/csp}. Suggestions & corrections welcome.

