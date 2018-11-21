#lang racket
(module+ test
  (require ptest/test0
           ptest/test1
           ptest/test2
           ptest/test3
           ptest/test4
           ptest/test5
           ptest/test6
           ptest/test7
           ptest/test8
           ptest/test9
           ptest/test10
           ptest/test11
           ptest/test12 ; ttf subset
           ptest/test13 ; subset with composites
           ptest/test14 ; Fira ttf with GPOS (no kerning)
           ptest/test15 ; Fira ttf with GPOS kerning
           ptest/test16 ; lig (GSUB)
           ptest/test17 ; multiple ligs & kerns (GSUB and GPOS)
           ptest/test18 ; paragraph
           pitfall/page-test
           (submod pitfall/zlib test)))