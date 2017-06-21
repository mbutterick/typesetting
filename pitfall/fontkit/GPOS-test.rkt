#lang fontkit/racket
(require fontkit rackunit restructure)

(define fira-path "../pitfall/test/assets/fira.ttf")
(define f (openSync fira-path))
(define ds (send f _getTableStream 'GPOS))

#|
(file-position (· ds _port))
(send uint32be decode (peek-bytes 4 0 (· ds _port))) ; version
(define ptr (send uint16be decode (peek-bytes 2 4 (· ds _port))))
ptr ; scriptList pointer
(send uint16be decode (peek-bytes 2 ptr (· ds _port))) ; number of Scriptrecords

(file-position (· ds _port) 0)
(define offset 692)
(send uint32be decode (peek-bytes 4 offset (· ds _port))) ; version
(define ptr2 (send uint16be decode (peek-bytes 2 (+ 4 offset) (· ds _port))))
ptr2 ; scriptList pointer
(send uint16be decode (peek-bytes 2 (+ ptr2 offset) (· ds _port))) ; number of Scriptrecords
|#

(report 'start-decode)
(define h (send GPOS decode (send f _getTableStream 'GPOS)))
h



;(check-equal? (· h version) #x00010000)

;h
;(check-equal? (length (· h scriptList)) 4)