#lang quad/typewriter

◊;quad[#:fontsize "11"]{Hello world}

◊;quad[#:fontsize "11"]{◊quad[#:link "http://beautifulracket.com"]{An expression that} is not a value can ◊quad[#:fontsize "22"]{always}  ◊quad[#:fontsize "7"]{be partitioned} into}


◊quad[#:fontsize "11"]{◊quad[#:link "http://beautifulracket.com"]{An expression that} is not a value can ◊quad[#:fontsize "22"]{always}  ◊quad[#:fontsize "7"]{be partitioned} into two parts: a redex, which is the part that changed in a single-step simplification (highlighted), and the continuation, which is the evaluation context surrounding an expression. In (- 4 (+ 1 1)), the redex is (+ 1 1), and the continuation is (- 4 []), where [] takes the place of the redex. That is, the continuation says how to "continue" after the redex is reduced to a value.}