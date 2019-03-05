#lang debug racket/base
(require racket/class xenomorph sugar/unstable/dict)
(provide CFFDict)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFDict.js
|#

(define CFFDict
  (class xenobase%
    (super-new)))