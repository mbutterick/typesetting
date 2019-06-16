  #lang scribble/manual

@(require scribble/eval (for-label racket/base racket/class racket/file racket/dict racket/list racket/stream racket/promise racket/vector xenomorph))

@(define my-eval (make-base-eval))
@(my-eval '(require xenomorph racket/list racket/stream racket/vector))


@title{Xenomorph: binary encoding & decoding}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@margin-note{This package is in development. I make no commitment to maintaining the public interface documented below.}

@defmodule[xenomorph]

Hands up: who likes working with binary formats?

OK, just a few of you, in the back. You're free to go. 

As for everyone else: Xenomorph eases the pain of working with binary formats. Instead of laboriously counting bytes —

@itemlist[#:style 'ordered
@item{You describe a binary format declaratively by using smaller ingredients — e.g., integers, strings, lists, pointers, dicts, and perhaps other nested encodings. This is known as a @deftech{xenomorphic object}.}

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



@section{The big picture}

@subsection{Bytes and byte strings}

Suppose we have a file on disk. What's in the file? Without knowing anything else, we can at least say the file contains a sequence of @deftech{bytes}. A @tech{byte} is the smallest unit of data storage. It's not, however, the smallest unit of information storage — that would be a @deftech{bit}. But when we read (or write) from disk (or other source, like memory), we work with bytes. 

A byte holds eight bits, so it can take on values between 0 and 255, inclusive. In Racket, a sequence of bytes is also known as a @deftech{byte string}. It prints as a series of values between quotation marks, prefixed with @litchar{#}:

@racketblock[#"ABC"]

Caution: though this looks similar to the ordinary string @racket["ABC"], we're better off thinking of it as a block of integers that are sometimes displayed as characters for convenience. For instance, the byte string above represents three bytes valued 65, 66, and 67. This byte string could also be written in hexadecimal like so:

@(racketvalfont "#\"\\x41\\x42\\x43\"")

Or octal like so:

@(racketvalfont "#\"\\101\\102\\103\"")

All three mean the same thing. (If you like, confirm this by trying them on the REPL.)

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

The key point: the @litchar{#} prefix tells us we're looking at a byte string, not an ordinary string.


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

@section{Tutorials}

@subsection{A binary format for complex numbers}

Racket natively supports @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{complex numbers}. Suppose we want to encode these numbers in a binary format without losing precision. How would we do it?

First, we need to understand Racket's recipe for a complex number:

@itemlist[
@item{A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{complex number} has a @italic{real part} and an @italic{imaginary part}. The coeffiecient of each part is a @italic{real number}.}

@item{A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{real number} is either a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{inexact number} (that is, a @italic{floating-point number}) or an @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{exact number}.}

@item{An @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{exact number} is a rational number — i.e., a number with a @italic{numerator} and @italic{denominator}.}

@item{The @italic{numerator} and @italic{denominator} can each be an arbitrarily large signed integer, which we'll call a @italic{big integer} to distinguish it from fixed-size integers otherwise common in binary formats.}

]

To make a binary format for complex numbers, we build the format by composing smaller ingredients into bigger ones. So we'll work the recipe from bottom to top, composing our ingredients as we go.

@subsubsection{Big integers}

Let's start with the big integers. We can't use an existing signed-integer type like @racket[int32] because our big integers won't necessarily fit. For that matter, this also rules out any type derived from @racket[x:int%], because all xenomorphic integers have a fixed size.

Instead, we need to use a variable-length type. How about an @racket[x:string]? If we don't specify a @racket[#:length] argument, it can be arbitrarily long. All we need to do is convert our number to a string before encoding (with @racket[number->string]) and then convert string to number after decoding (with @racket[string->number]).


@interaction[#:eval my-eval
(define bigint (x:string #:pre-encode number->string
                         #:post-decode string->number))

(define abigint (- (expt 2 80)))
abigint
(encode bigint abigint #f)
(decode bigint #"-1208925819614629174706176\0")
]

@subsubsection{Exact numbers}

Next, we handle exact numbers. An exact number is a combination of two big integers representing a numerator and a denominator. So in this case, we need a xenomorphic type that can store two values. How about an @racket[x:list]? The length of the list will be two, and the type of the list will be our new @racket[bigint] type.

Similar to before, we use pre-encoding to convert our Racket value into an encodable shape. This time, we convert an exact number into a list of its @racket[numerator] and @racket[denominator]. After decoding, we take that list and convert its values back into an exact number (by using @racket[/]):

@interaction[#:eval my-eval
(define exact (x:list #:type bigint 
                      #:length 2
                      #:pre-encode (λ (x) (list (numerator x) (denominator x)))
                      #:post-decode (λ (nd) (apply / nd))))
(encode exact -1234/5678 #f)
(decode exact #"-617\0002839\0")
]

@subsubsection{Real numbers}

A real number is either a floating-point number (for which we can use Xenomorph's built-in @racket[float] type) or an exact number (for which we can use the @racket[exact] type we just defined).

This time, we need an encoder that allows us to choose from among two possibilities. How about an @racket[x:versioned-dict]? We'll assign our exact numbers to version 0, and our floats to version 1. (These version numbers are arbitrary — we could pick any two values, but a small integer will fit inside a @racket[uint8].)

We specify a @racket[#:version-key] of @racket['version]. Then in our pre-encode function, we choose the version of the encoding based on whether the input value is @racket[exact?].

@interaction[#:eval my-eval
(define real (x:versioned-dict
              #:type uint8
              #:version-key 'version
              #:versions
              (list
               (cons 0 (list (cons 'val exact)))
               (cons 1 (list (cons 'val float))))
              #:pre-encode (λ (num) (list (cons 'val num)
                                          (cons 'version (if (exact? num)
                                                             0
                                                             1))))
              #:post-decode (λ (h) (hash-ref h 'val))))
(encode real 123.45 #f)
(decode real #"\1f\346\366B")
(encode real -1/16 #f)
(decode real #"\0-1\00016\0")
]

Notice that the float loses some precision during the encoding & decoding process. This is a natural part of how floating-point numbers work — they are called @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{inexact numbers} for this reason — so this is a feature, not a bug.

@subsubsection{Complex numbers}

Now we put it all together. A complex number is a combination of a real part and an imaginary part, each of which has a real coefficient. Therefore, we can model a complex number in a binary format just like we did for exact numbers: as a list of two values.

Once again, we use a pre-encoder and post-decoder to massage the data. On the way in, the pre-encoder turns the complex number into a list of real-number coefficients with @racket[real-part] and @racket[imag-part]. On the way out, these coefficients are reformed into a complex number through some easy addition and multiplication.


@interaction[#:eval my-eval
(define complex (x:list #:type real
                        #:length 2
                        #:pre-encode (λ (num) (list (real-part num) (imag-part num)))
                        #:post-decode (λ (ri) (+ (first ri) (* +i (second ri))))))
(encode complex 123.45-6.789i #f)
(decode complex #"\1f\346\366B\1}?\331\300")
(encode complex 1/234-5/678i #f)
(decode complex #"\0001\000234\0\0-5\000678\0")
]



@section{Main interface}

@defproc[
(xenomorphic?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:base%].
}

@defproc[
(decode
[xenomorphic-obj xenomorphic?]
[byte-source (or/c bytes? input-port?) (current-input-port)]
[#:parent parent (or/c xenomorphic? #false) #false]
[arg any/c] ...)
any/c]{
Read bytes from @racket[byte-source] and convert them to a Racket value using @racket[xenomorphic-obj] as the decoder.

If @racket[byte-source] contains more bytes than @racket[xenomorphic-obj] needs to decode a value, it reads as many bytes as necessary and leaves the rest.
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

If @racket[val] does not match the @racket[xenomorphic-obj] type appropriately — for instance, you try to @racket[encode] a negative integer using an unsigned integer type like @racket[uint8] — then an error will arise.
}


@section{Core xenomorphic objects}

These basic xenomorphic objects can be used on their own, or combined to make bigger xenomorphic objects.

Note on naming: the main xenomorphic objects have an @litchar{x:} prefix to distinguish them from (and prevent name collisions with) the ordinary Racket thing (for instance, @racket[x:list] vs. @racket[list]). Other xenomorphic objects (like @racket[uint8]) don't have this prefix, because it seems unnecessary and therefore laborious.

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


@subsubsection{Little endian vs. big endian}

When an integer is more than one byte long, one has to consider how the bytes are ordered. If the byte representing the lowest 8 bits appears first, it's known as @emph{little endian} byte ordering. If this byte appears last, it's called @emph{big endian} byte ordering. 

For example, the integer 1 in 32-bit occupies four bytes. In little endian, the bytes would be in increasing order, or @racket[#"\1\0\0\0"]. In big endian, the bytes would be in decreasing order, or @racket[#"\0\0\0\1"].

When encoding and decoding binary formats, one has to be consistent about endianness, because it will change the meaning of the binary value. For instance, if we inadvertently treated the big-endian byte string @racket[#"\0\0\0\1"] as little endian, we'd get the result @racket[16777216] instead of the expected @racket[1].


@defproc[
(endian-value?
[val any/c])
boolean?]{
Whether @racket[val] is either @racket['be] (representing big endian) or @racket['le] (representing little endian).
}


@defthing[system-endian endian-value?]{
The endian value of the current system. Big endian is represented as @racket['be] and little endian as @racket['le]. This can be used as an argument for classes that inherit from @racket[x:number%]. 

Use this value carefully, however. Binary formats are usually defined using one endian convention or the other (so that data can be exchanged among machines regardless of the endianness of the underlying system).

}

@defclass[x:number% x:base% ()]{
  
@defconstructor[
([size exact-positive-integer?]
[signed? boolean?]
[endian endian-value?])]{
Create class instance that represents a binary number format @racket[size] bytes long, either @racket[signed?] or not, with @racket[endian] byte ordering. The endian arugment can be @racket[system-endian].
}

}

@subsubsection{Integers}

@defclass[x:int% x:number% ()]{
Base class for integer formats. Use @racket[x:int] to conveniently instantiate new integer formats.
}

@defproc[
(x:int?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:int%].
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

@racket[size-arg] or @racket[size-kw] (whichever is provided, though @racket[size-arg] takes precedence) controls the encoded size.

@racket[signed] controls whether the integer is signed or unsigned.

@racket[endian] controls the byte-ordering convention.

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object.   
}

@deftogether[
(@defthing[int8 x:int?]
@defthing[int16 x:int?]
@defthing[int24 x:int?]
@defthing[int32 x:int?]
@defthing[int64 x:int?]
@defthing[uint8 x:int?]
@defthing[uint16 x:int?]
@defthing[uint24 x:int?]
@defthing[uint32 x:int?]
@defthing[uint64 x:int?])
]{
The common integer types, using @racket[system-endian] endianness. The @racket[u] prefix indicates unsigned. The numerical suffix indicates bit length.

Use these carefully, however. Binary formats are usually defined using one endian convention or the other (so that data can be exchanged among machines regardless of the endianness of the underlying system).

@examples[#:eval my-eval 
(encode int8 1 #f)
(encode int16 1 #f)
(encode int24 1 #f)
(encode int32 1 #f)
(encode int64 1 #f)
(encode int8 -128 #f)
(encode int16 -128 #f)
(encode int24 -128 #f)
(encode int32 -128 #f)
(encode int64 -128 #f)
(encode uint8 1 #f)
(encode uint16 1 #f)
(encode uint24 1 #f)
(encode uint32 1 #f)
(encode uint64 1 #f)
(code:comment @#,t{negative numbers cannot be encoded as unsigned ints, of course})
(encode uint8 -128 #f)
(encode uint16 -128 #f)
(encode uint24 -128 #f)
(encode uint32 -128 #f)
(encode uint64 -128 #f)
(decode int8 #"1" #f)
(decode int16 #"10" #f)
(decode int24 #"100" #f)
(decode int32 #"1000" #f)
(decode int64 #"10000000" #f)
(decode uint8 #"1" #f)
(decode uint16 #"10" #f)
(decode uint24 #"100" #f)
(decode uint32 #"1000" #f)
(decode uint64 #"10000000" #f)
]

}


@deftogether[
(@defthing[int8be x:int?]
@defthing[int16be x:int?]
@defthing[int24be x:int?]
@defthing[int32be x:int?]
@defthing[int64be x:int?]
@defthing[uint8be x:int?]
@defthing[uint16be x:int?]
@defthing[uint24be x:int?]
@defthing[uint32be x:int?]
@defthing[uint64be x:int?])
]{
Big-endian versions of the common integer types. The @racket[u] prefix indicates unsigned. The numerical suffix indicates bit length. @racket[int8be] and @racket[uint8be] are included for consistency, but as one-byte types, they are not affected by endianness.


@examples[#:eval my-eval 
(encode int8be 1 #f)
(encode int16be 1 #f)
(encode int24be 1 #f)
(encode int32be 1 #f)
(encode int64be 1 #f)
(encode int8be -128 #f)
(encode int16be -128 #f)
(encode int24be -128 #f)
(encode int32be -128 #f)
(encode int64be -128 #f)
(encode uint8be 1 #f)
(encode uint16be 1 #f)
(encode uint24be 1 #f)
(encode uint32be 1 #f)
(encode uint64be 1 #f)
(decode int8be #"1" #f)
(decode int16be #"10" #f)
(decode int24be #"100" #f)
(decode int32be #"1000" #f)
(decode int64be #"10000000" #f)
(decode int8be #"1" #f)
(decode int16be #"10" #f)
(decode int24be #"100" #f)
(decode int32be #"1000" #f)
(decode int64be #"10000000" #f)
(decode uint8be #"1" #f)
(decode uint16be #"10" #f)
(decode uint24be #"100" #f)
(decode uint32be #"1000" #f)
(decode uint64be #"10000000" #f)
]

}

@deftogether[
(@defthing[int8le x:int?]
@defthing[int16le x:int?]
@defthing[int24le x:int?]
@defthing[int32le x:int?]
@defthing[int64le x:int?]
@defthing[uint8le x:int?]
@defthing[uint16le x:int?]
@defthing[uint24le x:int?]
@defthing[uint32le x:int?]
@defthing[uint64le x:int?])
]{
Little-endian versions of the common integer types. The @racket[u] prefix indicates unsigned. The numerical suffix indicates bit length. @racket[int8le] and @racket[uint8le] are included for consistency, but as one-byte types, they are not affected by endianness.

@examples[#:eval my-eval 
(encode int8le 1 #f)
(encode int16le 1 #f)
(encode int24le 1 #f)
(encode int32le 1 #f)
(encode int64le 1 #f)
(encode int8le -128 #f)
(encode int16le -128 #f)
(encode int24le -128 #f)
(encode int32le -128 #f)
(encode int64le -128 #f)
(encode uint8le 1 #f)
(encode uint16le 1 #f)
(encode uint24le 1 #f)
(encode uint32le 1 #f)
(encode uint64le 1 #f)
(decode int8le #"1" #f)
(decode int16le #"10" #f)
(decode int24le #"100" #f)
(decode int32le #"1000" #f)
(decode int64le #"10000000" #f)
(decode uint8le #"1" #f)
(decode uint16le #"10" #f)
(decode uint24le #"100" #f)
(decode uint32le #"1000" #f)
(decode uint64le #"10000000" #f)
]
}


@subsubsection{Floats}

@defclass[x:float% x:number% ()]{
Base class for floating-point number formats. By convention, all floats are signed. Use @racket[x:float] to conveniently instantiate new floating-point number formats.
}

@defproc[
(x:float?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:float%].
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

@racket[size-arg] or @racket[size-kw] (whichever is provided, though @racket[size-arg] takes precedence) controls the encoded size.

@racket[endian] controls the byte-ordering convention.

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object.   
}

@deftogether[
(@defthing[float x:float?]
@defthing[floatbe x:float?]
@defthing[floatle x:float?])
]{
The common 32-bit floating-point types. They differ in byte-ordering convention: @racket[floatbe] uses big endian, @racket[floatle] uses little endian, @racket[float] uses @racket[system-endian].

@examples[#:eval my-eval 
(encode float 123.456 #f)
(encode floatbe 123.456 #f)
(encode floatle 123.456 #f)
(decode float #"y\351\366B" #f)
(decode floatbe #"y\351\366B" #f)
(decode floatle #"y\351\366B" #f)
]
}

@deftogether[
(@defthing[double x:float?]
@defthing[doublebe x:float?]
@defthing[doublele x:float?])
]{
The common 64-bit floating-point types. They differ in byte-ordering convention: @racket[doublebe] uses big endian, @racket[doublele] uses little endian, @racket[double] uses @racket[system-endian].

@examples[#:eval my-eval 
(encode double 123.456 #f)
(encode doublebe 123.456 #f)
(encode doublele 123.456 #f)
(decode double #"w\276\237\32/\335^@" #f)
(decode doublebe #"w\276\237\32/\335^@" #f)
(decode doublele #"w\276\237\32/\335^@" #f)
]
}


@subsubsection{Fixed-point numbers}


@defclass[x:fixed% x:int% ()]{
Base class for fixed-point number formats. Use @racket[x:fixed] to conveniently instantiate new fixed-point number formats.

@defconstructor[
([size exact-positive-integer?]
[signed? boolean?]
[endian endian-value?]
[fracbits exact-positive-integer?])]{
Create class instance that represents a fixed-point number format @racket[size] bytes long, either @racket[signed?] or not, with @racket[endian] byte ordering and @racket[fracbits] of precision.
}

}

@defproc[
(x:fixed?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:fixed%].
}

@defproc[
(x:fixed
[size-arg (or/c exact-positive-integer? #false) #false]
[#:size size-kw exact-positive-integer? 2]
[#:endian endian endian-value? system-endian]
[#:fracbits fracbits (or/c exact-positive-integer? #false) (/ (* _size 8) 2)]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:fixed%)) x:fixed%]
)
x:int?]{
Generate an instance of @racket[x:fixed%] (or a subclass of @racket[x:fixed%]) with certain optional attributes.

@racket[size-arg] or @racket[size-kw] (whichever is provided, though @racket[size-arg] takes precedence) controls the encoded size. Defaults to @racket[2].

@racket[endian] controls the byte-ordering convention.

@racket[fracbits] controls the number of bits of precision. If no value or @racket[#false] is passed, defaults to @racket[(/ (* _size 8) 2)].

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object.   
}

@deftogether[
(@defthing[fixed16 x:fixed?]
@defthing[fixed16be x:fixed?]
@defthing[fixed16le x:fixed?])
]{
The common 16-bit fixed-point number types with 2 bits of precision. They differ in byte-ordering convention: @racket[fixed16be] uses big endian, @racket[fixed16le] uses little endian, @racket[fixed16] uses @racket[system-endian].

Note that because of the limited precision, the byte encoding is possibly lossy (meaning, if you @racket[encode] and then @racket[decode], you may not get exactly the same number back).

@examples[#:eval my-eval 
(encode fixed16 123.45 #f)
(encode fixed16be 123.45 #f)
(encode fixed16le 123.45 #f)
(decode fixed16 #"s{" #f)
(decode fixed16be #"s{" #f)
(decode fixed16le #"s{" #f)
]
}

@deftogether[
(@defthing[fixed32 x:fixed?]
@defthing[fixed32be x:fixed?]
@defthing[fixed32le x:fixed?])
]{
The common 32-bit fixed-point number types with 4 bits of precision. They differ in byte-ordering convention: @racket[fixed32be] uses big endian, @racket[fixed32le] uses little endian, @racket[fixed32] uses @racket[system-endian].

Note that because of the limited precision, the byte encoding is possibly lossy (meaning, if you @racket[encode] and then @racket[decode], you may not get exactly the same number back).

@examples[#:eval my-eval 
(encode fixed32 123.45 #f)
(encode fixed32be 123.45 #f)
(encode fixed32le 123.45 #f)
(decode fixed32 #"3s{\0" #f)
(decode fixed32be #"3s{\0" #f)
(decode fixed32le #"3s{\0" #f)
]
}



@subsection{Strings}

@defmodule[xenomorph/string]

Good old strings. 

@defproc[
(supported-encoding?
[x any/c])
boolean?]{
Whether @racket[x] represents a supported encoding: either @racket['ascii] or @racket['utf8].
}

@defclass[x:string% x:base% ()]{
Base class for string formats. Use @racket[x:string] to conveniently instantiate new string formats.

@defconstructor[
([len length-resolvable?]
[encoding (or/c procedure? supported-encoding?)])]{
Create class instance that represents a string format of length @racket[len]. If @racket[len] is an integer,  the string is fixed at that length, otherwise it can be any length.
}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
string?]{
Returns a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{string}.
}

@defmethod[
#:mode extend
(x:encode
[val any/c]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Take a @racket[val], convert it to a string if needed, and encode it as a @tech{byte string}. If @racket[_len] is a @racket[xenomorphic?] object, the length is encoded at the beginning of the string using that object as the encoder.
}

}

@defproc[
(x:string?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:string%].
}

@defproc[
(x:string
[len-arg (or/c length-resolvable? #false) #false]
[enc-arg (or/c procedure? supported-encoding? #false) #false]
[#:length len-kw (or/c length-resolvable? #false) #false]
[#:encoding enc-kw (or/c procedure? supported-encoding? #false) 'utf8]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:string%)) x:string%]
)
x:string?]{
Generate an instance of @racket[x:string%] (or a subclass of @racket[x:string%]) with certain optional attributes.

@racket[len-arg] or @racket[len-kw] (whichever is provided, though @racket[len-arg] takes precedence) determines the maximum length in bytes of the encoded string. 

@itemlist[
@item{If this argument is an integer, the string is limited to that length. The length is not directly encoded.}

@item{If it's a @racket[xenomorphic?] type, the length is variable, but limited to the size that can be represented by that type. For instance, if @racket[len-arg] is @racket[uint8], then the string can be a maximum of 255 bytes. The length is encoded at the beginning of the byte string.}

@item{If it's another value, like @racket[#f], the string has variable length, and is null-terminated.}
]

@racket[enc-arg] or @racket[enc-kw] (whichever is provided, though @racket[enc-arg] takes precedence) determines the encoding of the string. Default is @racket['utf8]. See also @racket[supported-encoding?].


@examples[#:eval my-eval 
(define any-ascii (x:string #f 'ascii))
(encode any-ascii "ABC" #f)
(decode any-ascii #"ABC\0")
(decode any-ascii #"ABC\0DEF")
(decode any-ascii #"AB")
(define three-ascii (x:string 3 'ascii))
(encode three-ascii "ABC" #f)
(encode three-ascii "ABCD" #f)
(encode three-ascii "ABÜ" #f)
(decode three-ascii #"ABC")
(decode three-ascii #"ABCD")
(decode three-ascii (string->bytes/utf-8 "ABÜ"))
(define 256-utf8 (x:string uint8 'utf8)) 
(encode 256-utf8 "ABC" #f)
(encode 256-utf8 "ABCD" #f)
(encode 256-utf8 "ABÜ" #f) 
(encode 256-utf8 (make-string 256 #\A) #f) 
] 

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object. 


@examples[#:eval my-eval 
(define (doubler str) (string-append str str))
(define quad-str (x:string uint32be 
#:pre-encode doubler
#:post-decode doubler))
(encode quad-str "ABC" #f)
(decode quad-str #"\0\0\0\6ABCABC")
] 

}

@subsection{Symbols}

@defmodule[xenomorph/symbol]

Under the hood, just a wrapper around the @racket[x:string%] class.

@defclass[x:symbol% x:string% ()]{
Base class for symbol formats. Use @racket[x:symbol] to conveniently instantiate new symbol formats.

@defconstructor[
([len length-resolvable?]
[encoding (or/c procedure? supported-encoding?)])]{
Create class instance that represents a symbol format of length @racket[len]. If @racket[len] is an integer,  the symbol is fixed at that length, otherwise it can be any length.
}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
symbol?]{
Returns a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{symbol}.
}

@defmethod[
#:mode extend
(x:encode
[val any/c]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Take a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequence} @racket[seq] of @racket[_type] items and encode it as a @tech{byte string}.
}

}

@defproc[
(x:symbol?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:symbol%].
}

@defproc[
(x:symbol
[len-arg (or/c length-resolvable? #false) #false]
[enc-arg (or/c procedure? supported-encoding? #false) #false]
[#:length len-kw (or/c length-resolvable? #false) #false]
[#:encoding enc-kw (or/c procedure? supported-encoding? #false) 'utf8]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:symbol%)) x:symbol%]
)
x:symbol?]{
Generate an instance of @racket[x:symbol%] (or a subclass of @racket[x:symbol%]) with certain optional attributes, which are the same as @racket[x:string].

@examples[#:eval my-eval 
(define any-ascii (x:symbol #f 'ascii))
(encode any-ascii 'ABC #f)
(decode any-ascii #"ABC\0")
(decode any-ascii #"ABC\0DEF")
(decode any-ascii #"AB")
(define three-ascii (x:symbol 3 'ascii))
(encode three-ascii 'ABC #f)
(encode three-ascii 'ABCD #f)
(encode three-ascii 'ABÜ #f)
(decode three-ascii #"ABC")
(decode three-ascii #"ABCD")
(decode three-ascii (string->bytes/utf-8 "ABÜ"))
(define 256-utf8 (x:symbol uint8 'utf8)) 
(encode 256-utf8 'ABC #f)
(encode 256-utf8 'ABCD #f)
(encode 256-utf8 'ABÜ #f) 
(encode 256-utf8 (make-string 256 #\A) #f) 
(define (doubler sym)
(string->symbol (format "~a~a" sym sym)))
(define quad-str (x:symbol uint32be 
#:pre-encode doubler
#:post-decode doubler))
(encode quad-str "ABC" #f)
(decode quad-str #"\0\0\0\6ABCABC")
] 

}


@subsection{Lists}

@defmodule[xenomorph/list]

Lists in Xenomorph have a @emph{type} and maybe a @emph{length}. Every element in the list must have the same type. The list can have a specific length, but it doesn't need to (in which case the length is encoded as part of the data).

If you want to store items of different types in a single Xenomorph list, wrap them in @secref{Pointers} so they have a uniform type.

@defclass[x:list% x:base% ()]{
Base class for list formats. Use @racket[x:list] to conveniently instantiate new list formats.

@defconstructor[
([type xenomorphic?]
[len length-resolvable?]
[count-bytes? boolean?])]{
Create class instance that represents a list format with elements of type @racket[type]. If @racket[len] is an integer,  the list is fixed at that length, otherwise it can be any length. 
}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
list?]{
Returns a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{list} of values whose length is @racket[_len] and where each value is @racket[_type].
}

@defmethod[
#:mode extend
(x:encode
[seq sequence?]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Take a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequence} @racket[seq] of @racket[_type] items and encode it as a @tech{byte string}.
}

}

@defproc[
(x:list?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:list%].
}

@defproc[
(x:list
[type-arg (or/c xenomorphic? #false) #false]
[len-arg (or/c length-resolvable? #false) #false]
[#:type type-kw (or/c xenomorphic? #false) #false]
[#:length len-kw (or/c length-resolvable? #false) #false]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:list%)) x:list%]
)
x:list?]{
Generate an instance of @racket[x:list%] (or a subclass of @racket[x:list%]) with certain optional attributes.

@racket[type-arg] or @racket[type-kw] (whichever is provided, though @racket[type-arg] takes precedence) determines the type of the elements in the list.

@racket[len-arg] or @racket[len-kw] (whichever is provided, though @racket[len-arg] takes precedence) determines the length of the list. This can be an ordinary integer, but it can also be any value that is @racket[length-resolvable?].

@examples[#:eval my-eval 
(define three-uint8s (x:list uint8 3)) 
(encode three-uint8s '(1 2 3) #f)
(encode three-uint8s (string->bytes/utf-8 "ABC") #f)
(encode three-uint8s '(1 2 3 4) #f)
(encode three-uint8s '(1000 2000 3000) #f)
(encode three-uint8s '(A B C) #f)
(decode three-uint8s #"\1\2\3")
(decode three-uint8s #"\1\2\3\4")
(decode three-uint8s #"\1\2")
(define <256-uint8s (x:list #:type uint8 #:length uint8))
(encode <256-uint8s '(1 2 3) #f)
(encode <256-uint8s (make-list 500 1) #f)
(decode <256-uint8s #"\3\1\2\3")
(decode <256-uint8s #"\3\1\2\3\4")
(decode <256-uint8s #"\3\1\2")
(define nested (x:list #:type <256-uint8s #:length uint8))
(encode nested '((65) (66 66) (67 67 67)) #f)
(decode nested #"\3\1A\2BB\3CCC")
] 

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object.  


@examples[#:eval my-eval 
(define (doubler xs) (append xs xs))
(define quad-list (x:list uint16be 
#:pre-encode doubler
#:post-decode doubler))
(encode quad-list '(1 2 3) #f)
(decode quad-list #"\0\1\0\2\0\3\0\1\0\2\0\3")
]    
}


@subsection{Streams}

@defmodule[xenomorph/stream]

Under the hood, just a wrapper around the @racket[x:list%] class that produces a stream rather than a list. 

The distinguishing feature of a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{stream} is that the evaluation is lazy: elements are only decoded as they are requested (and then they are cached for subsequent use). Therefore, a Xenomorph stream is a good choice when you don't want to incur the costs of decoding every element immediately (as you will when you use @secref{Lists}). 

@defclass[x:stream% x:list% ()]{
Base class for stream formats. Use @racket[x:stream] to conveniently instantiate new stream formats.

@defconstructor[
([type xenomorphic?]
[len length-resolvable?]
[count-bytes? boolean?])]{
Create class instance that represents a stream format with elements of type @racket[type]. If @racket[len] is an integer,  the stream is fixed at that length, otherwise it can be any length. 
}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
stream?]{
Returns a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{stream} of values whose length is @racket[_len] and where each value is @racket[_type].
}

@defmethod[
#:mode extend
(x:encode
[seq sequence?]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Take a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequence} @racket[seq] of @racket[_type] items and encode it as a @tech{byte string}.
}

}

@defproc[
(x:stream?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:stream%].
}

@defproc[
(x:stream
[type-arg (or/c xenomorphic? #false) #false]
[len-arg (or/c length-resolvable? #false) #false]
[#:type type-kw (or/c xenomorphic? #false) #false]
[#:length len-kw (or/c length-resolvable? #false) #false]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:stream%)) x:stream%]
)
x:stream?]{
Generate an instance of @racket[x:stream%] (or a subclass of @racket[x:stream%]) with certain optional attributes, which are the same as @racket[x:list].

@examples[#:eval my-eval 
(define three-uint8s (x:stream uint8 3)) 
(encode three-uint8s '(1 2 3) #f)
(encode three-uint8s (string->bytes/utf-8 "ABC") #f)
(encode three-uint8s '(1 2 3 4) #f)
(encode three-uint8s '(1000 2000 3000) #f)
(encode three-uint8s '(A B C) #f)
(decode three-uint8s #"\1\2\3")
(decode three-uint8s #"\1\2\3\4")
(decode three-uint8s #"\1\2")
(define <256-uint8s (x:stream #:type uint8 #:length uint8))
(encode <256-uint8s '(1 2 3) #f)
(encode <256-uint8s (make-list 500 1) #f)
(stream->list (decode <256-uint8s #"\3\1\2\3"))
(for/list ([val (in-stream (decode <256-uint8s #"\3\1\2\3\4"))])
val)
(stream->list (decode <256-uint8s #"\3\1\2"))
(define (doubler xs) (append (stream->list xs) (stream->list xs)))
(define quad-stream (x:stream uint16be 
#:pre-encode doubler
#:post-decode doubler))
(encode quad-stream '(1 2 3) #f)
(decode quad-stream #"\0\1\0\2\0\3\0\1\0\2\0\3")
] 
}

@subsection{Vectors}

@defmodule[xenomorph/vector]

Under the hood, just a wrapper around the @racket[x:list%] class that decodes to a vector rather than a list.


@defclass[x:vector% x:list% ()]{
Base class for vector formats. Use @racket[x:vector] to conveniently instantiate new vector formats.

@defconstructor[
([type xenomorphic?]
[len length-resolvable?]
[count-bytes? boolean?])]{
Create class instance that represents a vector format with elements of type @racket[type]. If @racket[len] is an integer,  the vector is fixed at that length, otherwise it can be any length. 
}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
vector?]{
Returns a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{vector} of values whose length is @racket[_len] and where each value is @racket[_type].
}

@defmethod[
#:mode extend
(x:encode
[seq sequence?]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Take a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequence} @racket[seq] of @racket[_type] items and encode it as a @tech{byte string}.
}

}

@defproc[
(x:vector?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:vector%].
}

@defproc[
(x:vector
[type-arg (or/c xenomorphic? #false) #false]
[len-arg (or/c length-resolvable? #false) #false]
[#:type type-kw (or/c xenomorphic? #false) #false]
[#:length len-kw (or/c length-resolvable? #false) #false]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:vector%)) x:vector%]
)
x:vector?]{
Generate an instance of @racket[x:vector%] (or a subclass of @racket[x:vector%]) with certain optional attributes, which are the same as @racket[x:list].

@examples[#:eval my-eval 
(define three-uint8s (x:vector uint8 3)) 
(encode three-uint8s '#(1 2 3) #f)
(encode three-uint8s (string->bytes/utf-8 "ABC") #f)
(encode three-uint8s '(1 2 3 4) #f)
(encode three-uint8s '(1000 2000 3000) #f)
(encode three-uint8s '(A B C) #f)
(decode three-uint8s #"\1\2\3")
(decode three-uint8s #"\1\2\3\4")
(decode three-uint8s #"\1\2")
(define <256-uint8s (x:vector #:type uint8 #:length uint8))
(encode <256-uint8s '(1 2 3) #f)
(encode <256-uint8s (make-list 500 1) #f)
(vector->list (decode <256-uint8s #"\3\1\2\3"))
(for/list ([val (in-vector (decode <256-uint8s #"\3\1\2\3\4"))])
val)
(vector->list (decode <256-uint8s #"\3\1\2"))
(define (doubler vec) (vector-append vec vec))
(define quad-vec (x:vector uint16be 
#:pre-encode doubler
#:post-decode doubler))
(encode quad-vec '#(1 2 3) #f)
(decode quad-vec #"\0\1\0\2\0\3\0\1\0\2\0\3")
]   
}


@subsection{Dicts}

@defmodule[xenomorph/dict]

A @deftech{dict} is a store of keys and values. The analogy to a Racket @racket[dict?] is intentional, but in Xenomorph a dict must also be @emph{ordered}, because a binary encoding doesn't make sense if it happens in a different order every time. The more precise analogy would be to an @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{association list} — a thing that has both dict-like and list-like qualities — but this would be a laborious name. 

@defclass[x:dict% x:base% ()]{
Base class for dict formats. Use @racket[x:dict] to conveniently instantiate new dict formats.

@defconstructor[
([fields dict?])]{
Create class instance that represents a dict format with @racket[fields] as a dictionary holding the key–value pairs that define the dict format. Each key must be a @racket[symbol?] and each value must be a @racket[xenomorphic?] type.
}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
hash-eq?]{
Returns a @racket[hasheq] whose keys are the same as the keys in @racket[_fields].

}

@defmethod[
#:mode extend
(x:encode
[kvs dict?]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Take the keys and values in @racket[kvs] and encode them as a @tech{byte string}.
}

}

@defproc[
(x:dict?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:dict%].
}

@defproc[
(x:dict
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:dict%)) x:dict%]
[dict (listof (pairof symbol? xenomorphic?))] ...
[key symbol?] [val-type xenomorphic?] ... ...
)
x:dict?]{
Generate an instance of @racket[x:dict%] (or a subclass of @racket[x:dict%]) with certain optional attributes.

The rest arguments determine the keys and value types of the dict. These arguments can either be alternating keys and value-type arguments (similar to the calling pattern for @racket[hasheq]) or @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{association lists}.

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object.  

@examples[#:eval my-eval
(define d1 (x:dict 'foo uint8 'bar (x:string #:length 5)))
(define d1-vals (hasheq 'foo 42 'bar "hello"))
(encode d1 d1-vals #f)
(decode d1 #"*hello")

(define d2 (x:dict 'zam (x:list #:length 3 #:type uint8)
                   'nested d1))
(define d2-vals (hasheq 'zam '(42 43 44)
                        'nested d1-vals))
(encode d2 d2-vals #f)
(decode d2 #"*+,*hello")
]

}



@subsection{Versioned dicts}

@defmodule[xenomorph/versioned-dict]


The versioned dict is a format derived from @racket[x:dict%] that contains multiple possible dict encodings. It also carries a version field to select among them. This version is stored with the encoded data, of course, so on decode, the correct version will be chosen.


@defproc[
(version-type?
[x any/c])
boolean?]{
Whether @racket[x] can be used as the version type of a versioned dict. Valid types are @racket[integer?], @racket[procedure?], @racket[xenomorphic?], or @racket[symbol?].
}


@defclass[x:versioned-dict% x:dict% ()]{
Base class for versioned dict formats. Use @racket[x:versioned-dict] to conveniently instantiate new dict formats.

@defconstructor[
([type version-type?]
[versions dict?]
[version-key symbol?]
[fields #false])]{
Create class instance that represents a versioned dict format with @racket[type] as the encoded type of the version value, and @racket[versions] as a dictionary holding the key–value pairs that define the versioned dict. Each key of @racket[versions] must be a value consistent with @racket[type], and each value must either be a @racket[dict?] or @racket[x:dict?].
}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
hash-eq?]{
Returns a @racket[hasheq] whose keys are the same as the keys in @racket[_fields].

}

@defmethod[
#:mode extend
(x:encode
[kvs dict?]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Take the keys and values in @racket[kvs] and encode them as a @tech{byte string}.
}

}

@defproc[
(x:versioned-dict?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:versioned-dict%].
}

@defproc[
(x:versioned-dict
[type-arg (or/c version-type? #false)]
[versions-arg (or/c dict? #false)]
[#:type type-kw (or/c version-type? #false)]
[#:versions versions-kw (or/c dict? #false)]
[#:version-key version-key (or/c symbol? #false) x:version-key]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:versioned-dict%)) x:versioned-dict%]
)
x:versioned-dict?]{
Generate an instance of @racket[x:versioned-dict%] (or a subclass of @racket[x:versioned-dict%]) with certain optional attributes.

@racket[type-arg] or @racket[type-kw] (whichever is provided, though @racket[type-arg] takes precedence) determines the type of the version value that is used to select from among available dicts.

@racket[versions-arg] or @racket[versions-kw] (whichever is provided, though @racket[versions-arg] takes precedence) is a  dictionary holding the key–value pairs that define the versioned dict. Each key of @racket[versions] must be a value consistent with @racket[type], and each value must either be a @racket[dict?] or @racket[x:dict?].

@racket[version-key] identifies the key that should be treated as the version selector. By default, it's a separate private key called @racket[x:version-key] that exists independently of the data fields. But if one of the existing data fields should be treated as the version key, you can pass it as the @racket[version-key] argument. 

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object.  

@examples[#:eval my-eval
(define d1 (x:dict 'foo uint8 'bar (x:string #:length 5)))
(define d1-vals (hasheq 'foo 42 'bar "hello" 'my-version-key 'd1))

(define d2 (x:dict 'zam (x:list #:length 3 #:type uint8)
                   'nested d1))
(define d2-vals (hasheq 'zam '(42 43 44)
                        'nested d1-vals
                        'my-version-key 'd2))

(define vdict (x:versioned-dict
               #:type (x:symbol)
               #:version-key 'my-version-key
               #:versions (hash 'd1 d1 'd2 d2)))

(encode vdict d1-vals #f)
(decode vdict #"d1\0*hello")

(encode vdict d2-vals #f)
(decode vdict #"d2\0*+,*hello")
]

}

@subsubsection{Reserved values}

@declare-exporting[xenomorph]

@defthing[x:version-key symbol? #:value 'x:version]{
Key used by default to store & look up the version-selector value within the fields of a versioned dict. When the version dict is created, a different key can be specified.
}




@subsection{Pointers}

@defmodule[xenomorph/pointer]

A pointer can be thought of as a meta-object that can wrap any of the other binary formats here. It doesn't change how they work: they still take the same inputs (on @racket[encode]) and produce the same values (on @racket[decode]).

What it does change is the underlying housekeeping, by creating a layer of indirection around the data.

On @racket[encode], instead of storing the raw data at a certain point in the byte stream, it creates a reference — that is, a @deftech{pointer} — to that data at another location, and then puts the data at that location. 

On @racket[decode], the process is reversed: the pointer is dereferenced to discover the true location of the data, the data is read from that location, and then the decode proceeds as usual.

Under the hood, this housekeeping is fiddly and annoying. But good news! It's already been done. Please do something worthwhile with the hours of your life that have been returned to you.

Pointers can be useful for making data types of different sizes behave as if they were the same size. For instance, @secref{Lists} require all elements to have the same encoded size. What if you want to put different data types in the list? Wrap each item in a pointer, and you can make a list of pointers (because they have consistent size) that reference different kinds of data.



@defclass[x:pointer% x:base% ()]{
Base class for pointer formats. Use @racket[x:pointer] to conveniently instantiate new pointer formats.

@defproc[
(pointer-relative-value?
[x any/c])
boolean?]{
Whether @racket[x] can be used as a value for the @racket[_pointer-relative-to] field of @racket[x:pointer%]. Valid choices are @racket['(local immediate parent global)].
}

@defconstructor[
([ptr-type x:int?]
[dest-type (or/c xenomorphic? 'void)]
[pointer-relative-to pointer-relative-value?]
[allow-null? boolean?]
[null-value any/c]
[pointer-lazy? boolean?])]{
Create class instance that represents a pointer format. See @racket[x:pointer] for a description of the fields.



}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
any/c]{
Returns the dereferenced value of the pointer whose type is controlled by @racket[_dest-type].
}

@defmethod[
#:mode extend
(x:encode
[val any/c]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Take a value of type @racket[_dest-type], wrap it in a pointer, and encode it as a @tech{byte string}.
}

}

@defproc[
(x:pointer?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:pointer%].
}

@defproc[
(x:pointer
[ptr-type-arg (or/c x:int? #false) #false]
[dest-type-arg (or/c xenomorphic? 'void #false) #false]
[#:type ptr-type-kw (or/c x:int? #false) uint32]
[#:dest-type dest-type-kw (or/c xenomorphic? 'void #false) uint8]
[#:relative-to pointer-relative-to pointer-relative-value? 'local]
[#:allow-null allow-null? boolean? #true]
[#:null null-value any/c 0]
[#:lazy pointer-lazy? boolean? #false]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:pointer%)) x:pointer%]
)
x:pointer?]{
Generate an instance of @racket[x:pointer%] (or a subclass of @racket[x:pointer%]) with certain optional attributes.

@racket[ptr-type-arg] or @racket[ptr-type-kw] (whichever is provided, though @racket[ptr-type-arg] takes precedence) controls the type of the pointer value itself, which must be an @racket[x:int?]. Default is @racket[uint32].

@racket[dest-type-arg] or @racket[dest-type-kw] (whichever is provided, though @racket[dest-type-arg] takes precedence)  controls the type of the thing being pointed at, which must be a @racket[xenomorphic?] object  or the symbol @racket['void] to indicate a void pointer. Default is @racket[uint8].


@racket[pointer-relative-to] controls how the byte-offset value stored in the pointer is calculated. It must be one of @racket['(local immediate parent global)]. Default is @racket['local].

@racket[allow-null?] controls whether the pointer can take on null values, and @racket[null-value] controls what that value is. Defaults are @racket[#true] and @racket[0], respectively.

@racket[pointer-lazy?] controls whether the pointer is decoded immediately. If @racket[pointer-lazy?] is @racket[#true], then the decoding of the pointer is wrapped in a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{promise} that can later be evaluated with @racket[force]. Default is @racket[#false].

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object.   
}

@subsubsection{Private values}

@declare-exporting[xenomorph]

@deftogether[(@defthing[x:start-offset-key symbol? #:value 'x:start-offset]
@defthing[x:current-offset-key symbol? #:value 'x:current-offset]
@defthing[x:parent-key symbol? #:value 'x:parent]
@defthing[x:pointer-size-key symbol? #:value 'x:ptr-size]
@defthing[x:pointers-key symbol? #:value 'x:pointers]
@defthing[x:pointer-offset-key symbol? #:value 'x:ptr-offset]
@defthing[x:pointer-type-key symbol? #:value 'x:ptr-type]
@defthing[x:length-key symbol? #:value 'x:length]
@defthing[x:val-key symbol? #:value 'x:val])]{

Private fields used for pointer housekeeping. There is no reason to mess with these.
}





@subsection{Bitfields}

@defmodule[xenomorph/bitfield]

A @deftech{bitfield} is a compact encoding for Boolean values using an integer, where each bit of the integer indicates @racket[#true] or @racket[#false] (corresponding to a value of @racket[1] or @racket[0]). The bitfield object creates a mapping between the keys of the bitfield (called @deftech{flags}) and the integer bits.

@defclass[x:bitfield% x:base% ()]{
Base class for bitfield formats. Use @racket[x:bitfield] to conveniently instantiate new bitfield formats.


@defconstructor[
([type x:int?]
[flags (listof (or/c symbol? #false))])]{
Create class instance that represents a bitfield format. See @racket[x:bitfield] for a description of the fields.

}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
hash?]{
Returns a hash whose keys are the names of the flags, and whose values are Booleans.
}

@defmethod[
#:mode extend
(x:encode
[flag-hash hash?]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Take a hash — where hash keys are the names of the flags, hash values are Booleans — and encode it as a @tech{byte string}. 
}

}

@defproc[
(x:bitfield?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:bitfield%].
}

@defproc[
(x:bitfield
[type-arg (or/c x:int? #false) #false]
[flags-arg (listof any/c)]
[#:type type-kw (or/c x:int? #false) uint8]
[#:flags flags-kw (listof any/c) null]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:bitfield%)) x:bitfield%]
)
x:bitfield?]{
Generate an instance of @racket[x:bitfield%] (or a subclass of @racket[x:bitfield%]) with certain optional attributes.

@racket[type-arg] or @racket[type-kw] (whichever is provided, though @racket[type-arg] takes precedence) controls the type of the bitfield value itself, which must be an @racket[x:int?]. Default is @racket[uint8].

@racket[flags-arg] or @racket[flags-kw] (whichever is provided, though @racket[flags-arg] takes precedence) is a list of flag names corresponding to each bit. The number of names must be fewer than the number of bits in @racket[_type]. No name can be duplicated. Each flag name can be any value, but @racket[#false] indicates a skipped bit. Default is @racket[null].

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object.   

@examples[#:eval my-eval
(define flags (x:bitfield uint8 '(alpha bravo charlie delta echo)))
(define vals (hasheq
              'alpha #true
              'charlie #true
              'echo #true))
(encode flags vals #f)
(decode flags #"\25")
]

}


@subsection{Enumerations}

@defmodule[xenomorph/enum]

An @deftech{enumeration} is a mapping of values to sequential integers.


@defclass[x:enum% x:base% ()]{
Base class for list formats. Use @racket[x:enum] to conveniently instantiate new enumeration formats.

@defconstructor[
([type x:int?]
[values (listof any/c)])]{
Create class instance that represents an enumeration format of type @racket[type], sequentially mapped to @racket[values]. 
}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
any/c]{
Returns either the value associated with a certain integer, or if the value is @racket[#false] or doesn't exist, then the integer itself.
}

@defmethod[
#:mode extend
(x:encode
[val any/c]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Take value listed in the @racket[_values] field and encode it as a @tech{byte string}.
}

}

@defproc[
(x:enum?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:enum%].
}

@defproc[
(x:enum
[type-arg (or/c x:int? #false) #false]
[values-arg (listof any/c) #false]
[#:type type-kw (or/c x:int? #false) uint8]
[#:values values-kw (listof any/c) null]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:enum%)) x:enum%]
)
x:enum?]{
Generate an instance of @racket[x:enum%] (or a subclass of @racket[x:enum%]) with certain optional attributes.

@racket[type-arg] or @racket[type-kw] (whichever is provided, though @racket[type-arg] takes precedence) determines the integer type for the enumeration. Default is @racket[uint8].

@racket[values-arg] or @racket[values-kw] (whichever is provided, though @racket[values-arg] takes precedence) determines the mapping of values to integers, where each value corresponds to its index in the list. @racket[#false] indicates skipped values. Default is @racket[null].

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object.

@examples[#:eval my-eval
(define e (x:enum #:type uint8
                  #:values '("foo" "bar" "baz" #f)))
(encode e "baz" #f)
(decode e #"\2")
(code:comment @#,t{corresponding enum value is #false, so we pass through input value})
(decode e #"\3")
(code:comment @#,t{no corresponding enum value, so we pass through input value})
(decode e #"\5")
]

}



@;{

@subsection{Optional}

@defmodule[xenomorph/optional]

A wrapper format that decodes or encodes only if the embedded condition evaluates to true.


@defclass[x:optional% x:base% ()]{
Base class for optional formats. Use @racket[x:optional] to conveniently instantiate new optional formats.


@defconstructor[
([type xenomorphic?]
[condition any/c])]{
Create class instance that represents an optional format. See @racket[x:optional] for a description of the fields.

}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
hash?]{
Returns a value if the condition is met, otherwise returns @racket[(void)].
}

@defmethod[
#:mode extend
(x:encode
[val any/c]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Encodes @racket[val] as a @tech{byte string}, but only if the embedded condition is met. 
}

}

@defproc[
(x:optional?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:optional%].
}

@defproc[
(x:optional
[type-arg (or/c xenomorphic? #false) #false]
[cond-arg any/c]
[#:type type-kw (or/c xenomorphic? #false)]
[#:condition cond-kw any/c #true]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:optional%)) x:optional%]
)
x:optional?]{
Generate an instance of @racket[x:optional%] (or a subclass of @racket[x:optional%]) with certain optional attributes.

@racket[type-arg] or @racket[type-kw] (whichever is provided, though @racket[type-arg] takes precedence) controls the type wrapped by the optional object, which must be @racket[xenomorphic?].

@racket[cond-arg] or @racket[cond-kw] (whichever is provided, though @racket[cond-arg] takes precedence) is the condition that is evaluated to determine if the optional object should encode or decode. 

If the condition is a procedure, the procedure is evaluated for its result. The procedure must take two arguments: the first is the optional object, the second is the parent object (if it exists). Default is @racket[#true].

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object.   
}

}



@subsection{Reserved}

@defmodule[xenomorph/reserved]

The reserved object simply skips data. The advantage of using a reserved object rather than the type itself is a) it clearly signals that the data is being ignored, and b) it prevents writing to that part of the data structure.

@defclass[x:reserved% x:base% ()]{
Base class for reserved formats. Use @racket[x:reserved] to conveniently instantiate new reserved formats.


@defconstructor[
([type xenomorphic?]
[count exact-positive-integer?])]{
Create class instance that represents an reserved format. See @racket[x:reserved] for a description of the fields.

}

@defmethod[
#:mode extend
(x:decode
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
void?]{
Returns @racket[(void)].
}

@defmethod[
#:mode extend
(x:encode
[val any/c]
[input-port input-port?]
[parent (or/c xenomorphic? #false)])
bytes?]{
Encodes zeroes as a @tech{byte string} that is the length of @racket[_type].
}

}

@defproc[
(x:reserved?
[x any/c])
boolean?]{
Whether @racket[x] is an object of type @racket[x:reserved%].
}

@defproc[
(x:reserved
[type-arg (or/c xenomorphic? #false) #false]
[count-arg (or/c exact-positive-integer? #false)]
[#:type type-kw (or/c xenomorphic? #false)]
[#:count count-kw exact-positive-integer? 1]
[#:pre-encode pre-encode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:post-decode post-decode-proc (or/c (any/c . -> . any/c) #false) #false]
[#:base-class base-class (λ (c) (subclass? c x:reserved%)) x:reserved%]
)
x:reserved?]{
Generate an instance of @racket[x:reserved%] (or a subclass of @racket[x:reserved%]) with certain optional attributes.

@racket[type-arg] or @racket[type-kw] (whichever is provided, though @racket[type-arg] takes precedence) controls the type wrapped by the reserved object, which must be @racket[xenomorphic?].

@racket[count-arg] or @racket[count-kw] (whichever is provided, though @racket[count-arg] takes precedence) is the number of items of @racket[_type] that should be skipped.

@racket[pre-encode-proc] and @racket[post-decode-proc] control the pre-encoding and post-decoding procedures, respectively. Each takes as input the value to be processed and returns a new value.

@racket[base-class] controls the class used for instantiation of the new object. 

@examples[#:eval my-eval
(define res (x:reserved #:type uint32))
(encode res 1 #f)
(encode res 1234 #f)
(encode res 12345678 #f)
(void? (decode res #"\0\0\0\0"))
]

}

@subsection{Utilities}

@defmodule[xenomorph/util]

@defproc[
(length-resolvable?
[x any/c])
boolean?]{
Whether @racket[x] is something that can be used as a length argument with @racket[xenomorphic?] objects that have length. For instance, an @racket[x:list] or @racket[x:stream].

The following values are deemed to be resolvable: any @racket[exact-nonnegative-integer?], an @racket[x:int?], or any @racket[procedure?] that takes one argument (= the parent object) returns a @racket[exact-nonnegative-integer?].
}

@section{License & source code}

This module is licensed under the MIT license.


Source repository at @link["http://github.com/mbutterick/xenomorph"]{http://github.com/mbutterick/xenomorph}. Suggestions & corrections welcome.

