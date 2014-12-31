#lang racket/base
(require "quads.rkt" racket/file racket/string racket/function racket/list)
(provide (all-defined-out))

;(define ti (block '(measure 54 leading 18) "Meg is an ally."))
(define (ti2) (block '(leading 10 measure 400  size 13 x-align left x-align-last-line left font "Equity Text B") (block #f "Foo-d" (word '(size 13) "og ") "and " (box) "Zu" (word-break '(nb "c" bb "k-")) "kerman’s. Instead of a circle, the result is a picture of the code that, if it were used as an expression, would produce a circle. In other words, code is not a function, but instead a " (block '(style italic) "new syntactic form") " for creating pictures; the bit between the opening " (block '(weight bold) "parenthesis") " with code is not an expression, but instead manipulated by the code syntactic form. " (word '(font "Triplicate T4" size 22.5 color "Orchid" background "Yellow") "Bangazoom!") " This helps explain what we meant in the previous section when we said that racket provides require and the function-calling syntax.") (block-break) (block #f "Libraries are not restricted to exporting values, such as functions; they can also define new syntactic forms. In this sense, Racket isn’t exactly a language at all; it’s more of an idea for how to structure a language so that you can extend it or create entirely new ones.")))

(define (ti3) (block '(measure 54 leading 18) "Meg is an ally."))

(define (ti4) (block '(measure 300 x-align justify x-align-last-line right leading 18) "In this Madagascarian hoo-ha, Racket isn’t exactly a language at all"))


(define (ti5) (block '(measure 240 font "Equity Text B" leading 16 size 13.5 x-align justify x-align-last-line left) (box '(width 15)) (block #f (block '(weight bold font "Equity Caps B") "Hotdogs, My Fellow Americans.") " This " (block '(no-break #t) "is some truly") " bullshit generated from my typesetting system, which is called Quad. I’m writing this in a source file in DrRacket. When I click [Run], a PDF pops out. Not bad\u200a—\u200aand no LaTeX needed. Quad, however, does use the fancy linebreaking algorithm developed for TeX. (It also includes a faster linebreaking algorithm for when speed is more important than quality.) Of course, it can also handle " (block '(font "Triplicate C4") "different fonts,") (block '(style italic) " styles, ") (word '(size 14 weight bold) "and sizes-") " within the same line. As you can see, it can also justify paragraphs." (block-break) (box '(width 15)) (block #f "“Each horizontal row represents an OS-level thread, and the colored dots represent important events in the execution of the program (they are color-coded to distinguish one event type from another). The upper-left blue dot in the timeline represents the future’s creation. The future executes for a brief period (represented by a green bar in the second line) on thread 1, and then pauses to allow the runtime thread to perform a future-unsafe operation.") (block-break)  (box '(width 15))(block #f "In the Racket implementation, future-unsafe operations fall into one of two categories. A blocking operation halts the evaluation of the future, and will not allow it to continue until it is touched. After the operation completes within touch, the remainder of the future’s work will be evaluated sequentially by the runtime thread. A synchronized operation also halts the future, but the runtime thread may perform the operation at any time and, once completed, the future may continue running in parallel. Memory allocation and JIT compilation are two common examples of synchronized operations."))))

(define (ti6) (block '(font "Equity Text B" measure 210 leading 14 size 20 x-align justify x-align-last-line left)
                   "Firstlinerhere" (column-break) "Secondlinerhere" (column-break) "Thirdlinerhere"))


(define (make-jude jude-text)
  (define jude-blocks (map (λ(s) (regexp-replace* #rx"\n" s " ")) (string-split (file->string jude-text) "\n\n")))
  (apply block '(font "Equity Text B" measure 360 leading 14 column-count 1 column-gutter 10 size 11.5 x-align justify x-align-last-line left) (add-between (map (λ(jb) (block #f (box '(width 10)) (optical-kern) jb)) jude-blocks) (block-break))))

(define (jude) (make-jude "texts/jude.txt"))
(define (jude0) (make-jude "texts/jude0.txt"))
(define (judebig) (make-jude "texts/judebig.txt"))


(define (jude1) (block '(font "Equity Text B" measure 150 leading 14 column-count 4 size 11 x-align justify x-align-last-line left) "this—is—a—test—of—em—dashes—breakable—or—not?"))