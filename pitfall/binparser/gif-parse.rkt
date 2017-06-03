#lang br
(require pitfall/binprint binparser)

;; http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp


(define-rule gif (:seq signature version logical-screen-descriptor #:type hash?))
(define-rule signature (:atomic 3 #:type string/ascii?))
(define-rule version (:atomic 3 #:type string/ascii?))

(define-rule logical-screen-descriptor (:seq width height lsd-flags bgcolor-idx aspect #:type hash?))
(define-rule width (:atomic 2 #:type integer?))
(define-rule height (:atomic 2 #:type integer?))
(define-rule lsd-flags (:seq reserved disposal user-input transparent #:type hash?))
(define-rule reserved (:atomic .3))
(define-rule disposal (:atomic .3))
(define-rule user-input (:atomic .1))
(define-rule transparent (:atomic .1))
(define-rule bgcolor-idx (:atomic 1 #:type integer?))
(define-rule aspect (:atomic 1 #:type integer?))



(gif (open-input-file "test.gif"))


#;(check-equal? (gif (gif (open-input-file "test.gif"))) (read-bytes 13 (open-input-file "test.gif")))






(require rackunit)
#;(check-equal? (parse-with-template "test.gif" gif)
                (cons 'gif
                      (make-hasheq (list (cons 'logical-screen-descriptor '(162 162 (#f #t #f #f #f #t #f #t) 0 0))
                                         '(signature . "GIF")
                                         '(version . "87a")))))
