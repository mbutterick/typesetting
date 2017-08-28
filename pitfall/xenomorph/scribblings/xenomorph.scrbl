#lang scribble/manual

@(require scribble/eval (for-label racket xenomorph))

@(define my-eval (make-base-eval))
@(my-eval `(require xenomorph))


@title{Xenomorph: binary encoding & decoding}

@author[(author+email "Matthew Butterick" "mb@mbtype.com") "Devon Govett"]

@defmodule[xenomorph]

Hands up: who likes parsing and writing binary formats?

OK, just a few of you, in the back. You're free to go. 

Everyone else: Xenomorph eases the pain of working with binary formats. Instead of fiddling with counting bytes:

@itemlist[#:style 'ordered
@item{You define an @deftech{encoding} describing the binary format using smaller ingredients — e.g., integers, strings, arrays, pointers, and sub-encodings.}

@item{This encoding can then be used as a binary compiler, converting Racket values to binary and writing them out to a file.}

@item{But wait, there's more: this encoding can @emph{also} be used as a binary parser, reading bytes and parsing them into Racket values. So one encoding definition can be used for both input and output.}
]

Derived principally from Devon Govett's @link["https://github.com/devongovett/restructure"]{@tt{restructure}} library for Node. Thanks for doing the heavy lifting, dude.


@section{Installation}

At the command line:

@verbatim{raco pkg install xenomorph}

After that, you can update the package from the command line:

@verbatim{raco pkg update xenomorph}

Invoke the library in a source file by importing it in the usual way:

@verbatim{(require xenomorph)}



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



@section{The big picture}

@subsection{Bytes and byte strings}

Suppose we have a file on disk. What's in the file? Without knowing anything else, we can at least say the file contains a sequence of @deftech{bytes}. A @deftech{byte} is the smallest unit of data storage. It's not, however, the smallest unit of information storage — that would be a @deftech{bit}. But when we read (or write) from disk (or other source, like memory), we work with bytes. 

A byte holds eight bits, so it can take on values between 0 and 255, inclusive. In Racket, a sequence of bytes is also known as a @deftech{byte string}. It prints as a series of values between quotation marks, prefixed with @litchar{#}:

@racketblock[#"ABC"]

Caution: though this looks similar to the ordinary string @racket["ABC"], we're better off thinking of it as a sequence of integers that are sometimes displayed as characters for convenience. For instance, the byte string above represents three bytes valued 65, 66, and 67. This byte string could also be written in hexadecimal like so:

@(racketvalfont "#\"\\x41\\x42\\x43\"")

Or octal like so:

@(racketvalfont "#\"\\101\\102\\103\"")

Both of these mean the same thing. (If you like, confirm this by trying them on the REPL.)

We can also make an equivalent byte string with @racket[bytes]. As above, Racket doesn't care how we notate the values, as long as they're between 0 and 255:

@bold{TODO}: escape the chars below

@examples[#:eval my-eval
(bytes 65 66 67)
(bytes #x41 #x42 #x43)
(bytes #o101 #o102 #o103)
(apply bytes (map char->integer '(#\A #\B #\C)))
]

Byte values between 32 and 127 are printed as characters. Other values are printed in octal:

@examples[#:eval my-eval
(bytes 65 66 67 154 206 255)
]

If you think this printing convention is a little weird, I agree. But that's how Racket does it. If we prefer to deal with lists of integers, we can always use @racket[bytes->list] and @racket[list->bytes]:

@examples[#:eval my-eval
(bytes->list #"ABC\232\316\377")
(list->bytes '(65 66 67 154 206 255))
]

The important thing is that when we see the @litchar{#"} prefix, we know we're looking at a byte string, not an ordinary string.


@subsection{Encodings}

Back to files. Typically, files on disk are classified as being either @deftech{binary} or @deftech{text}. (A distinction observed by Racket functions such as @racket[write-to-file].) When we speak of binary vs. text, we're saying something about the internal structure of the byte sequence — what values those bytes represent. This internal structure is also called an @deftech{encoding}. An encoding is a way of representing a sequence of arbitrary values as a sequence of bytes.

@subsubsection{Text encodings}

Text files are a just a particular subset of binary files that use a @deftech{text encoding} — that is, a binary encoding that stores human-readable characters. 

But since we all have experience with text files, let's use text encoding as a way of starting to understand what's happening under the hood with binary encodings.

For example, ASCII is a familiar encoding that stores each character in seven bits, so it can describe 128 distinct characters. Because every ASCII code is less than 255, we can store ASCII text with one byte per character.

But if we want to use more than 128 distinct characters, we're stuck. That's why Racket instead uses the UTF-8 text encoding by default. UTF-8 uses between one and three bytes to encode each character, and can thus represent up to 1,112,064 distinct characters. We can see how this works by converting a string into an encoded byte sequence using @racket[string->bytes/utf-8]:

@examples[#:eval my-eval
(string->bytes/utf-8 "ABCD")
(bytes->list (string->bytes/utf-8 "ABCD"))
(string->bytes/utf-8 "ABÇ战")
(bytes->list (string->bytes/utf-8 "ABÇ战"))
]

For ASCII-compatible characters, UTF-8 uses one byte for each character. Thus, the string @racket["ABCD"] is four bytes long in UTF-8. 

Now consider the string @racket["ABÇ战"], which has four characters, but the second two aren't ASCII-compatible. In UTF-8, it's encoded as seven bytes: the first two characters are one byte each, the @racket["Ç"] takes two bytes, and the @racket["战"] takes three.

Moreover, for further simplicity, text files typically rely on a small set of pre-defined encodings, like ASCII or UTF-8 or Latin-1, so that those who write programs that manipulate text only have to support a smallish set of encodings.

@subsubsection{Binary encodings}



@subsubsection{In sum}

Three corollaries follow:

@itemlist[#:style 'ordered
@item{A given sequence of bytes can mean different things, depending on what encoding we use.}

@item{We can only make sense of a sequence of bytes if we know its encoding.}

@item{A byte sequence does not describe its own encoding.}

]

For those familiar with programming-language lingo, an encoding somewhat resembles a @deftech{grammar}, which is a tool for describing the syntactic structure of a program. A grammar doesn't describe one particular program. Rather, it describes all possible programs that are consistent with the grammar, and therefore can be used to parse any particular one. Likewise for an encoding.

@margin-note{Can a grammar work as a binary encoding? In limited cases, but not enough to be practical. Most grammars have to assume the target program is context free, meaning that the grammar rules apply the same way everywhere. By contrast, binary files are nonrecursive and contextual.}




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

