#lang at-exp br
(require rackunit "parser.rkt" "tokenizer.rkt" brag/support)

(apply-tokenizer-maker make-tokenizer @string-append{(string () here) << /A (B) >>})

#;(parse-to-datum (apply-tokenizer-maker make-tokenizer @string-append{(string () here) << /A (B) >>}))