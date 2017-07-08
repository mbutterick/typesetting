#lang racket
(module+ test
  (require pitfall/test/test0
           pitfall/test/test1
           pitfall/test/test2
           pitfall/test/test3
           pitfall/test/test4
           pitfall/test/test5
           pitfall/test/test6
           pitfall/test/test7
           pitfall/test/test8
           pitfall/test/test09
           pitfall/test/test10
           pitfall/test/test11
           pitfall/test/test12 ; ttf subset
           pitfall/test/test13 ; subset with composites
           pitfall/test/test14 ; Fira ttf with GPOS (no kerning)
           pitfall/test/test15 ; Fira ttf with GPOS kerning
           pitfall/test/test16 ; lig (GSUB)
           pitfall/test/test17 ; multiple ligs & kerns (GSUB and GPOS)
           pitfall/test/test18 ; paragraph
           pitfall/page-test
           (submod pitfall/zlib test)))