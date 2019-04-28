#lang scribble/manual

@(require scribble/eval (for-label racket/base racket/class racket/file xenomorph))

@(define my-eval (make-base-eval))
@(my-eval `(require xenomorph))


@title{Xenomorph: binary encoding & decoding}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@margin-note{This package is in development. I make no commitment to maintaining the public interface documented below.}

@defmodule[xenomorph]

Hands up: who likes working with binary formats?

OK, just a few of you, in the back. You're free to go. 

As for everyone else: Xenomorph eases the pain of working with binary formats. Instead of laboriously counting bytes —

@itemlist[#:style 'ordered
@item{You describe a binary format declaratively by using smaller ingredients — e.g., integers, strings, lists, pointers, structs, and perhaps other nested encodings. This is known as a @deftech{xenomorphic object}.}

@item{This xenomorphic object can then be used as a binary encoder, allowing you to convert Racket values to binary and write them out to a file.}

@item{But wait, there's more: once defined, this xenomorphic object can @emph{also} be used as a binary decoder, reading bytes and parsing them into Racket values.}
]

So one binary-format definition can be used for both input and output. Meanwhile, Xenomorph handles all the dull housekeeping of counting bytes (because somebody has to).

This package is derived principally from Devon Govett's @link["https://github.com/devongovett/restructure"]{@tt{restructure}} library for Node.js. Thanks for doing the heavy lifting, dude.


@section{Installation}

At the command line:

@verbatim{raco pkg install xenomorph}

After that, you can update the package from the command line:

@verbatim{raco pkg update xenomorph}

Invoke the library in a source file by importing it in the usual way:

@verbatim{(require xenomorph)}


@;{
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


}


@section{The big picture}

@subsection{Bytes and byte strings}

Suppose we have a file on disk. What's in the file? Without knowing anything else, we can at least say the file contains a sequence of @deftech{bytes}. A @tech{byte} is the smallest unit of data storage. It's not, however, the smallest unit of information storage — that would be a @deftech{bit}. But when we read (or write) from disk (or other source, like memory), we work with bytes. 

A byte holds eight bits, so it can take on values between 0 and 255, inclusive. In Racket, a sequence of bytes is also known as a @deftech{byte string}. It prints as a series of values between quotation marks, prefixed with @litchar{#}:

@racketblock[#"ABC"]

Caution: though this looks similar to the ordinary string @racket["ABC"], we're better off thinking of it as a block of integers that are sometimes displayed as characters for convenience. For instance, the byte string above represents three bytes valued 65, 66, and 67. This byte string could also be written in hexadecimal like so:

@(racketvalfont "#\"\\x41\\x42\\x43\"")

Or octal like so:

@(racketvalfont "#\"\\101\\102\\103\"")

Both of these mean the same thing. (If you like, confirm this by trying them on the REPL.)

We can also make an equivalent byte string with @racket[bytes]. As above, Racket doesn't care how we notate the values, as long as they're between 0 and 255:

@examples[#:eval my-eval
(bytes 65 66 67)
(bytes (+ 31 34) (* 3 22) (- 100 33))
(apply bytes (map char->integer '(#\A #\B #\C)))
]

Byte values between 32 and 127 are printed as characters. Other values are printed in octal:

@examples[#:eval my-eval
(bytes 65 66 67 154 206 255)
]

If you think this printing convention is a little weird, I agree. But that's how Racket does it. 

If we prefer to deal with lists of integers, we can always use @racket[bytes->list] and @racket[list->bytes]:

@examples[#:eval my-eval
(bytes->list #"ABC\232\316\377")
(list->bytes '(65 66 67 154 206 255))
]

The key point: the @litchar{#"} prefix tells us we're looking at a byte string, not an ordinary string.


@subsection{Binary formats}

Back to files. Files are classified as being either @deftech{binary} or @deftech{text}. (A distinction observed by Racket functions such as @racket[write-to-file].) When we speak of binary vs. text, we're saying something about the internal structure of the byte sequence — what values those bytes represent. We'll call this internal structure the @deftech{binary format} of the file.

@margin-note{This internal structure is also called an @emph{encoding}. Here, however, I avoid using that term as a synonym for @tech{binary format}, because I prefer to reserve it for when we talk about encoding and decoding as operations on data.}

@;{
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

}


@section{Main interface}

@defproc[
(xenomorphic?
[x any/c])
boolean?]{
Predicate for whether @racket[x] is an object of type @racket[x:base%].
}

@defproc[
(decode
[xenomorphic-obj xenomorphic?]
[byte-source (or/c bytes? input-port?) (current-input-port)]
[#:parent parent (or/c xenomorphic? #false) #false]
[arg any/c] ...)
any/c]{
Read bytes from @racket[byte-source] and convert them to a Racket value using @racket[xenomorphic-obj] as the decoder.
}

@defproc[
(encode
[xenomorphic-obj xenomorphic?]
[val any/c]
[byte-dest (or/c output-port? #false) (current-output-port)]
[#:parent parent (or/c xenomorphic? #false) #false]
[arg any/c] ...
)
(or/c void? bytes?)]{
Convert @racket[val] to bytes using @racket[xenomorphic-obj] as the encoder. 

If @racket[byte-dest] is an @racket[output-port?], the bytes are written there and the return value is @racket[(void)]. If @racket[byte-dest] is @racket[#false], the encoded byte string is the return value.
}

@defproc[
(size
[xenomorphic-obj xenomorphic?]
[val any/c #false]
[#:parent parent (or/c xenomorphic? #false) #false]
[arg any/c] ...)
exact-nonnegative-integer?]{
The length of the @tech{byte string} that @racket[val] would produce if it were encoded using @racket[encode].
}


@section{Core xenomorphic objects}

These basic xenomorphic objects can be used on their own, or combined to make bigger xenomorphic objects.

Note on naming: the main xenomorphix objects have an @litchar{x:} prefix to distinguish them from (and prevent name collisions with) the ordinary Racket thing (for instance, @racket[x:list] vs. @racket[list]). Other xenomorphic objects (like @racket[uint8]) don't have this prefix, because it seems unnecessary and therefore laborious.

@defclass[x:base% object% ()]{

When making your own xenomorphic objects, usually you'll want to stick together existing core objects, or inherit from one of those classes. Inheriting from @racket[x:base%] is also allowed, but you have to do all the heavy lifting.

@defmethod[
#:mode pubment
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)]
[args any/c] ...)
any/c]{
Read bytes from @racket[input-port] and convert them into a Racket value. Called by @racket[decode].
}

@defmethod[
(post-decode
[val any/c])
any/c]{
Hook for post-processing on @racket[val] after it's returned by @racket[x:decode] but before it's returned by @racket[decode].
}

@defmethod[
#:mode pubment
(x:encode
[val any/c]
[output-port output-port?]
[parent (or/c xenomorphic? #false)]
[args any/c] ...)
bytes?]{
Convert a value into a @tech{byte string} which is written to @racket[output-port]. Called by @racket[encode].
}

@defmethod[
(pre-encode
[val any/c])
any/c]{
Hook for pre-processing on @racket[val] after it's passed to @racket[encode] but before it's passed to @racket[x:encode].
}

@defmethod[
#:mode pubment
(x:size
[val any/c]
[parent (or/c xenomorphic? #false)]
[args any/c] ...)
exact-nonnegative-integer?]{
The length of the @tech{byte string} that @racket[val] would produce if it were encoded using @racket[x:encode]. Called by @racket[size].
}

}

@subsection{Numbers}

@defmodule[xenomorph/number]

@defproc[
(endian-value?
[val any/c])
boolean?]{
Whether @racket[val] is either @racket['be] (representing big endian) or @racket['le] (representing little endian).
}


@defthing[system-endian endian-value?]{
The endian value of the current system. Big endian is represented as @racket['be] and little endian as @racket['le]. This can be used as an argument for the @racket[x:number%] constructor. 

Use this value carefully, however, as most binary formats are defined using one endian convention or the other (so that they can be exchanged among systems regardless of endianness).
}

@defclass[x:number% x:base% ()]{
  
@defconstructor[
([size exact-positive-integer?]
[endian endian-value?])]{
Create class instance that represents a binary number format @racket[size] bytes long with @racket[endian] byte ordering. The endian arugment can be @racket[system-endian].
}

}

@subsubsection{Integers}

@defclass[x:int% x:number% ()]{
Base class for integer objects. Use @racket[x:int] to conveniently instantiate new integer objects.
}

@defproc[
(x:int?
[x any/c])
boolean?]{
Predicate for whether @racket[x] is an object of type @racket[x:int%].
}

@defproc[
(x:int
[size-arg (or/c exact-positive-integer? #false) #false]
[#:size size-kw exact-positive-integer? 2]
[#:signed signed boolean? #true]
[#:endian endian endian-value? system-endian]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:int%)) x:int%]
)
x:int?]{
Generate an instance of @racket[x:int%] (or a subclass of @racket[x:int%]) with certain optional attributes.

@racket[size-arg] or @racket[size-kw] (whichever is provided, though @racket[size-kw] takes precedence) controls the encoded size.

@racket[signed] controls whether the integer is signed or unsigned.

@racket[endian] controls the byte-ordering convention.

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decodeing procedures, respectively.

@racket[base-class] controls the class used for instantiation of the new object.   
}

@deftogether[
(@defthing[int8 x:int?]
@defthing[int16 x:int?]
@defthing[int24 x:int?]
@defthing[int32 x:int?]
@defthing[uint8 x:int?]
@defthing[uint16 x:int?]
@defthing[uint24 x:int?]
@defthing[uint32 x:int?])
]{
The common integer types, using @racket[system-endian] endianness. The @racket[u] prefix indicates unsigned. The numerical suffix indicates bit length.

Use these carefully, however, as most binary formats are defined using one endian convention or the other (so that they can be exchanged among systems regardless of endianness).
}

@deftogether[
(@defthing[int8be x:int?]
@defthing[int16be x:int?]
@defthing[int24be x:int?]
@defthing[int32be x:int?]
@defthing[uint8be x:int?]
@defthing[uint16be x:int?]
@defthing[uint24be x:int?]
@defthing[uint32be x:int?])
]{
Big-endian versions of the common integer types. The @racket[u] prefix indicates unsigned. The numerical suffix indicates bit length.
}

@deftogether[
(@defthing[int8le x:int?]
@defthing[int16le x:int?]
@defthing[int24le x:int?]
@defthing[int32le x:int?]
@defthing[uint8le x:int?]
@defthing[uint16le x:int?]
@defthing[uint24le x:int?]
@defthing[uint32le x:int?])
]{
Little-endian versions of the common integer types. The @racket[u] prefix indicates unsigned. The numerical suffix indicates bit length.
}


@subsubsection{Floats}

@defclass[x:float% x:number% ()]{
Base class for floating-point number objects. By convention, all floats are signed. Use @racket[x:float] to conveniently instantiate new floating-point number objects.
}

@defproc[
(x:float?
[x any/c])
boolean?]{
Predicate for whether @racket[x] is an object of type @racket[x:float%].
}

@defproc[
(x:float
[size-arg (or/c exact-positive-integer? #false) #false]
[#:size size-kw exact-positive-integer? 2]
[#:endian endian endian-value? system-endian]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:float%)) x:float%]
)
x:int?]{
Generate an instance of @racket[x:float%] (or a subclass of @racket[x:float%]) with certain optional attributes.

@racket[size-arg] or @racket[size-kw] (whichever is provided, though @racket[size-kw] takes precedence) controls the encoded size.

@racket[endian] controls the byte-ordering convention.

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decodeing procedures, respectively.

@racket[base-class] controls the class used for instantiation of the new object.   
}

@deftogether[
(@defthing[float x:float?]
@defthing[floatbe x:float?]
@defthing[floatle x:float?])
]{
The common 32-bit floating-point types. They differ in byte-ordering convention: @racket[floatbe] uses big endian, @racket[floatle] uses little endian, @racket[float] uses @racket[system-endian].
}

@deftogether[
(@defthing[double x:float?]
@defthing[doublebe x:float?]
@defthing[doublele x:float?])
]{
The common 64-bit floating-point types. They differ in byte-ordering convention: @racket[doublebe] uses big endian, @racket[doublele] uses little endian, @racket[double] uses @racket[system-endian].
}


@subsubsection{Fixed-points}


@subsection{Strings}

@defmodule[xenomorph/string]

@subsection{Lists}

@defmodule[xenomorph/list]

@subsection{Streams}

@defmodule[xenomorph/stream]

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
TK
}


@section{License & source code}

This module is licensed under the MIT license.


Source repository at @link["http://github.com/mbutterick/xenomorph"]{http://github.com/mbutterick/xenomorph}. Suggestions & corrections welcome.

