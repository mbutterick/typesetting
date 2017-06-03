;			Binary parsing

;----------------------------------------
; Apologia
;
; Binary parsing and unparsing are transformations between primitive or
; composite Scheme values and their external binary representations.
;
; Examples include reading and writing JPEG, TIFF, MP3, ELF file
; formats, communicating with DNS, Kerberos, LDAP, SLP internet
; services, participating in Sun RPC and CORBA/IIOP distributed systems,
; storing and retrieving (arrays of) floating-point numbers in a
; portable and efficient way. This project will propose a set of low- and
; intermediate- level procedures that make binary parsing possible.

; Scheme is a good language to do research in text compression. Text
; compression involves a great deal of building and traversing
; dictionaries, trees and similar data structures, where Scheme
; excels. Performance doesn't matter in research, but the size of
; compressed files does (to figure out the bpc for the common
; benchmarks). Variable-bit i/o is a necessity. It is implemented
; in the present file.

; ASN.1 corresponds to a higher-level parsing (LR parser
; vs. lexer). Information in LDAP responses and X.509 certificates is
; structural and recursive, and so lends itself to be processed in
; Scheme. Variable bit i/o is necessary, and so is a binary lexer for
; a LR parser. Parsing of ASN.1 is a highly profitable enterprise

;----------------------------------------
; The outline of the project
;
; Primitives and streams
;
; - read-byte 
; - read-u8vector (cf. read-string)
; - with-input-from-u8vector, with-input-from-encoded-u8vector 'base64,... 
; building binary i/o streams from a sequence of bytes. Streams over
; u8vector, u16vector, etc. provide a serial access to memory. See SRFI-4
;
; - read-bit, read-bits via overlayed streams given read-byte
; implemented in the present file.
;
; -  mmap-u8vector, munmap-u8vector
;
; Conversions
;  - u8vector->integer u8vector endianness,
;    u8vector->sinteger u8vector endianness
;  These conversion procedures turn a sequence of bytes to an unsigned or
;  signed integer, minding the byte order. The u8vector in question can
;  have size 1,2,4,8, 3 etc. bytes. These two functions therefore can be
;  used to read shorts, longs, extra longs, etc. numbers.
;  - u8vector-reverse and other useful u8vector operations
; 
;  - modf, frexp, ldexp
;  The above primitives can be emulated in R5RS, yet they are quite handy
;  (for portable FP manipulation) and can be executed very efficiently by
;  an FPU.
;
; Higher-level parsing and combinators
; These are combinators that can compose primitives above for more
; complex (possibly iterative) actions. 
;
; - skip-bits, next-u8token,...
; - IIOP, RPC/XDR, RMI
; - binary lexer for existing LR/LL-parsers
; 
; The composition of primitives and combinators will represent binary
; parsing language in a _full_ notation. This is similar to XPath
; expressions in full notation. Later we need to find out the
; most-frequently used patterns of the binary parsing language and
; design an abbreviated notation. The latter will need a special
; "interpreter". The abbreviated notation may turn out to look like
; Olin's regular expressions.

; $Id: binary-read.scm,v 1.1 2000/10/20 17:49:47 oleg Exp oleg $


;----------------------------------------
; Test harness
;
; The following macro runs built-in test cases -- or does not run,
; depending on which of the two lines below you commented out
(define-macro (run-test . body) `(begin (display "\n-->Test\n") ,@body))
;(define-macro (run-test . body) '(begin #f))
;(defmacro run-test body `(begin (display "\n-->Test\n") ,@body))

;;========================================================================
;;			Configuration section
;;
; Performance is very important for binary parsing. We have to get all
; help from a particular Scheme system we can get. If a Scheme function
; can support the following primitives faster, we should take
; advantage of that fact.


;---
; Configuration for Gambit. See below for other systems, as well as R5RS
; implementations
(declare 
 (block)
 (standard-bindings)
)

(define-macro (logior x y) `(##fixnum.logior ,x ,y))
(define-macro (logand x y) `(##fixnum.logand ,x ,y))
(define-macro (lsh-left x n) `(##fixnum.shl ,x ,n))
(define-macro (lsh-right x n) `(##fixnum.lshr ,x ,n))
(define-macro (lsh-left-one x) `(##fixnum.shl ,x 1))
(define-macro (lsh-right-one x) `(##fixnum.lshr ,x 1))

(define-macro (-- x)	`(##fixnum.- ,x 1))
(define-macro (++ x)	`(##fixnum.+ ,x 1))

(define-macro (bit-set? x mask)		; return x & mask != 0
  `(##not (##fixnum.zero? (logand ,x ,mask)))
)
; End of the Gambit-specific configuration section
;---

; combine bytes in the MSB order. A byte may be #f
(define (combine-two b1 b2)		; The result is for sure a fixnum
  (and b1 b2 (logior (lsh-left b1 8) b2)))

(define (combine-three b1 b2 b3)	; The result is for sure a fixnum
  (and b1 b2 b3 (logior (lsh-left (logior (lsh-left b1 8) b2) 8) b3)))

; Here the result may be a BIGNUM
(define (combine-bytes . bytes)
  (cond 
   ((null? bytes) 0)
   ((not (car bytes)) #f)
   (else 
    (let loop ((bytes (cdr bytes)) (result (car bytes)))
      (cond
       ((null? bytes) result)
       ((not (car bytes)) #f)
       (else (loop (cdr bytes) (+ (car bytes) (* 256 result)))))))))

;---
; R5RS implementations of the primitives
; This is the most portable -- and the slowest implementation

; See also logical.scm from SLIB
; (define (logior x y)
;   (cond ((= x y) x)
; 	((zero? x) y)
; 	((zero? y) x)
; 	(else
; 	 (+ (* (logior (quotient x 2) (quotient y 2)) 2)
; 	    (if (and (even? x) (even? y)) 0 1)))))
; (define (logand x y)
;   (cond ((= x y) x)
; 	((zero? x) 0)
; 	((zero? y) 0)
; 	(else
; 	 (+ (* (logand (quotient x 2) (quotient y 2)) 2)
; 	    (if (or (even? x) (even? y)) 0 1)))))

; (define (lsh-left x n) (* x (expt 2 n)))
; (define (lsh-right x n) (quotient x (expt 2 n)))
; (define (lsh-left-one x) (* x 2))
; (define (lsh-right-one x) (quotient x 2))


; (define (-- x)	(- x 1))
; (define (++ x)	(+ x 1))

; (define (bit-set? x mask)		; return x & mask != 0
;   (odd? (quotient x mask))		; mask is an exact power of two
; )


;========================================================================
;			   Reading a byte 

; Read-byte is a fundamental primitive; it needs to be
; added to the standard. Most of the other functions are library
; procedures. The following is an approximation, which clearly doesn't
; hold if the port is a Unicode (especially UTF-8) character stream.

; Return a byte as an exact integer [0,255], or the EOF object
(define (read-byte port)
  (let ((c (read-char port)))
    (if (eof-object? c) c (char->integer c))))

; The same as above, but returns #f on EOF.
(define (read-byte-f port)
  (let ((c (read-char port)))
    (and (not (eof-object? c)) (char->integer c))))


  
;========================================================================
;			Bit stream

; -- Function: make-bit-reader BYTE-READER

; Given a BYTE-READER (a thunk), construct and return a function
;	bit-reader N
;
; that reads N bits from a byte-stream represented by the BYTE-READER.
; The BYTE-READER is a function that takes no arguments and returns
; the current byte as an exact integer [0-255]. The byte reader
; should return #f on EOF.
; The bit reader returns N bits as an exact unsigned integer, 
; 0 -... (no limit). N must be a positive integer, otherwise the bit reader
; returns #f. There is no upper limit on N -- other than the size of the
; input stream itself and the amount of (virtual) memory an OS is willing
; to give to your process. If you want to read 1M of _bits_, go ahead.
;
; It is assumed that the bit order is the most-significant bit first.
;
; Note the bit reader keeps the following condition true at all times:
;	(= current-inport-pos (ceiling (/ no-bits-read 8)))
; That is, no byte is read until the very moment we really need (some of)
; its bits. The bit reader does _not_ "byte read ahead".
; Therefore, it can be used to handle a concatenation of different
; bit/byte streams *STRICTLY* sequentially, _without_ 'backing up a char',
; 'unreading-char' etc. tricks.
; For example, make-bit-reader has been used to read GRIB files of
; meteorological data, which made of several bitstreams with headers and
; tags.
; Thus careful attention to byte-buffering and optimization are the
; features of this bit reader.
;
; Usage example:
;	(define bit-reader (make-bit-reader (lambda () #b11000101)))
;	(bit-reader 3) ==> 6
;	(bit-reader 4) ==> 2
; The test driver below is another example.
;
; Notes on the algorithm.
; The function recognizes and handles the following special cases:
;  - the buffer is empty and 8, 16, 24 bits are to be read
;  - reading all bits which are currently in the byte-buffer
;    (and then maybe more)
;  - reading only one bit

; Since the bit reader is going to be called many times, optimization is
; critical. We need all the help from the compiler/interpreter
; we can get.


(define (make-bit-reader byte-reader)
  (let ((buffer 0) (mask 0)     ; mask = 128 means that the buffer is full and
                                ; the msb bit is the current (yet unread) bit
	(bits-in-buffer 0))

       ; read the byte into the buffer and set up the counters.
       ; return #f on eof
       (define (set-buffer)
	 (set! buffer (byte-reader))
	 (and buffer
	      (begin
		(set! bits-in-buffer 8)
		(set! mask 128)
		#t)))

       ; Read fewer bits than there are in the buffer
       (define (read-few-bits n)
	 (let ((value (logand buffer 	; all bits in buffer
			      (-- (lsh-left-one mask)))))
	   (set! bits-in-buffer (- bits-in-buffer n))
	   (set! mask (lsh-right mask n))
	   (lsh-right value bits-in-buffer))) ; remove extra bits

      ; read n bits given an empty buffer, and append them to value, n>=8
       (define (add-more-bits value n)
	 (let loop ((value value) (n n))
	   (cond
	    ((zero? n) value)
	    ((< n 8)
	     (let ((rest (read-n-bits n)))
	       (and rest (+ (* value (lsh-left 1 n)) rest))))
	    (else
	     (let ((b (byte-reader)))
	       (and b (loop (+ (* value 256) b) (- n 8))))))))

	    ; The main module
       (define (read-n-bits n)
	 ; Check the most common cases first
	 (cond
	  ((not (positive? n)) #f)
	  ((zero? bits-in-buffer)	; the bit-buffer is empty
	   (case n
	     ((8) (byte-reader))
	     ((16) 
	      (let ((b (byte-reader)))
		(combine-two b (byte-reader))))
	     ((24)
	      (let* ((b1 (byte-reader)) (b2 (byte-reader)))
		(combine-three b1 b2 (byte-reader))))
	     (else
	      (cond
	       ((< n 8)
		(and (set-buffer) (read-few-bits n)))
	       ((< n 16)
		(let ((b (byte-reader)))
		  (and (set-buffer)
		    (logior (lsh-left b (- n 8))
			    (read-few-bits (- n 8))))))
	       (else
		(let ((b (byte-reader)))
		  (and b (add-more-bits b (- n 8)))))))))

	  ((= n 1)			; read one bit
	   (let ((value (if (bit-set? buffer mask) 1 0)))
	     (set! mask (lsh-right-one mask))
	     (set! bits-in-buffer (-- bits-in-buffer))
	     value))

	  ((>= n bits-in-buffer)	; will empty the buffer
	   (let ((n-rem (- n bits-in-buffer))
		 (value (logand buffer 	; for mask=64, it'll be &63
				(-- (lsh-left-one mask)))))
	     (set! bits-in-buffer 0)
	     (cond
	      ((zero? n-rem) value)
	      ((<= n-rem 16)
	       (let ((rest (read-n-bits n-rem)))
		 (and rest (logior (lsh-left value n-rem) rest))))
	      (else (add-more-bits value n-rem)))))
	  (else (read-few-bits n))
	  ))

       read-n-bits)
  )


; Validation tests

(run-test
 (define (read-bits numbers nbits)
   (let* ((left-numbers numbers)
	  (bit-reader
	   (make-bit-reader 
	    (lambda ()
	      (and (pair? left-numbers)
		   (let ((byte (car left-numbers)))
		     (set! left-numbers (cdr left-numbers))
		     byte))))))
     (let loop ((result '()))
       (let ((num (bit-reader nbits)))
	 (if num (loop (cons num result)) (reverse result))))))
 (define (do-test numbers nbits expected)
   (let ((result (read-bits numbers nbits)))
     (for-each display
	(list "Reading " numbers " by " nbits " bits\n"
	      "The result is: " result "\n"))
     (or (equal? result expected)
	  (error "the result differs from the expected: " expected))))

 (do-test '(1 2 3 4 5 6 7) 8 '(1 2 3 4 5 6 7))
 (do-test '(193 5 131 4) 1 
	  '(1 1 0 0  0 0 0 1  0 0 0 0  0 1 0 1  1 0 0 0  0 0 1 1
	    0 0 0 0  0 1 0 0))
 (do-test '(193 5 131 4 5) 2
	  '(3 0 0 1  0 0 1 1  2 0 0 3  0 0 1 0 0 0 1 1))
 (do-test '(193 5 131 4) 3 
	  '(6 0 2 0  2 6 0 3  0 1))
 (do-test '(193 5 131 4 5 6 7) 4
	  '(12 1 0 5 8 3 0 4 0 5 0 6 0 7))
 (do-test '(193 5 131 4 5 6 7) 5
	  '(24 4 2 24 6 1 0 5 0 24 3))
 (do-test '(193 5 131 4 5 6 7 8 17 24) 8
	   '(193 5 131 4 5 6 7 8 17 24))
 (do-test '(193 5 131 4 5 6 7 8 17 24) 9
	  '(386 22 24 64 160 385 388 17))
 (do-test '(193 5 131 4 5 6 7 8 17) 16
	  '(49413 33540 1286 1800))
 (do-test '(193 5 131 4 5 6 104) 17
	  '(98827 3088 10291))
 (do-test '(193 5 131 4 5 6 104) 19
	  '(395308 49409))
 (do-test '(193 5 131 4 5 6 104) 55
	  '(27165365385724724))
 (do-test '(193 5 131 4 5 6 104) 56
	  '(54330730771449448))
)


; Timing test
; This test relies on a Gambit special form 'time' to clock
; evaluation of an expression.
; R5RS does not provide any timing facilities. So the test below
; might not run on your particular system, and probably needs
; adjustment anyway.

(run-test
 (let ((fname "/tmp/a") (size 10240)
       (pattern (integer->char #x55)))
   (define (read-by-bits n)
     (for-each display
       (list "Reading the file by " n " bits "))
     (call-with-input-file fname
       (lambda (port)
	 (let ((bit-reader (make-bit-reader
			    (lambda () (read-byte-f port)))))
	   (time
	    (do ((c (bit-reader n) (bit-reader n))) ((not c))))))))

   (for-each display
     (list "Creating a file " fname " of size " size " filled with "
	   pattern "\n"))
   (with-output-to-file fname 
     (lambda () (do ((i 0 (+ 1 i))) ((>= i size)) (write-char pattern))))

   (display "\nReading the file by characters: the baseline ")
   (call-with-input-file fname
     (lambda (port)
       (time 
	(do ((c (read-char port) (read-char port))) ((eof-object? c))))))

   (display "\nReading the file by bytes: ")
   (call-with-input-file fname
     (lambda (port)
       (time 
	(do ((c (read-byte-f port) (read-byte-f port))) ((not c))))))

   (for-each read-by-bits
     (list 1 2 3 4 5 6 7 8 9 10 11 15 16 17 23 24 25 30 32 65535
	   (* 8 size)))
))
