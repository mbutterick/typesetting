#lang fontkit/racket
(require fontkit rackunit restructure)

(define fira-path "../pitfall/test/assets/fira.ttf")
(define f (openSync fira-path))
(define ds (send f _getTableStream 'GPOS))

(file-position (· ds _port))
(peek-bytes 4 0 (· ds _port)) ; version
(peek-bytes 2 4 (· ds _port)) ; scriptList pointer
(peek-bytes 2 10 (· ds _port)) ; number of Scriptrecords

(send uint16be decode #"\0\n")

(define h (send GPOS decode ds))



;(check-equal? (· h version) #x00010000)

;h
;(check-equal? (length (· h scriptList)) 4)