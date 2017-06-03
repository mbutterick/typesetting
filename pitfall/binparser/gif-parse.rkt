#lang br
(require pitfall/binprint binparser)

;; http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp

(define-rule gif (:seq [signature (:bytes 3 #:type string/ascii?)]
                       [version (:bytes 3 #:type string/ascii?)]
                       logical-screen-descriptor
                       #:type hash?))

(define-rule logical-screen-descriptor (:seq [width (:bytes 2 #:type integer?)]
                                             [height (:bytes 2 #:type integer?)]
                                             [lsd-flags (:bitfield [reserved (:bits 3)]
                                                                   [disposal (:bits 3 #:type integer?)]
                                                                   [user-input (:bits 1 #:type boolean?)]
                                                                   [transparent (:bits 1 #:type boolean?)]
                                                                   #:type hash?)]
                                             [bgcolor-idx (:bytes 1 #:type integer?)]
                                             [aspect (:bytes 1 #:type integer?)]
                                             #:type hash?))

(gif (open-input-file "test.gif"))

(check-equal? (gif (gif (open-input-file "test.gif"))) (read-bytes 13 (open-input-file "test.gif")))

(require rackunit)
#;(check-equal? (parse-with-template "test.gif" gif)
                (cons 'gif
                      (make-hasheq (list (cons 'logical-screen-descriptor '(162 162 (#f #t #f #f #f #t #f #t) 0 0))
                                         '(signature . "GIF")
                                         '(version . "87a")))))


(define-rule bad-bitfield (:bitfield [reserved (:bits 3)]
                                     [disposal (:bits 3 #:type integer?)]))

(bad-bitfield (bad-bitfield (open-input-bytes #"A")))