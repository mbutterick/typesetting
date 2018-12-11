#lang racket/base
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Array.coffee
|#



(test-module
 (check-equal? (decode (+Array uint16be 3) #"ABCDEF") '(16706 17220 17734))
 (check-equal? (encode (+Array uint16be 3) '(16706 17220 17734) #f) #"ABCDEF")
 (check-equal? (size (+Array uint16be) '(1 2 3)) 6)
 (check-equal? (size (+Array doublebe) '(1 2 3 4 5)) 40))
