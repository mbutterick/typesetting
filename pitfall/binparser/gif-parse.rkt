#lang br
(require pitfall/binprint binparser racket/dict)

;; http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp

(define-rule gif (:seq [signature (:bytes 3 #:type string/ascii?)]
                       [version (:bytes 3 #:type string/ascii?)]
                       logical-screen-descriptor
                       global-color-table
                       #:type assoc?))

(define-rule logical-screen-descriptor (:seq [width (:bytes 2 #:type integer?)]
                                             [height (:bytes 2 #:type integer?)]
                                             [lsd-flags (:seq [global-color-table-size (:bits 3 #:type integer?)]
                                                              [sort (:bits 1 #:type boolean?)]
                                                              [resolution (:bits 3 #:type integer?)]
                                                              [global-color-table (:bits 1 #:type integer?)]
                                                              #:type assoc?)]
                                             [bgcolor-idx (:bytes 1 #:type integer?)]
                                             [aspect (:bytes 1 #:type integer?)]
                                             #:type assoc?))

(define-rule global-color-table (:repeat 4 (:bytes 3)))

#;(define-rule color (:bytes 3 #:type hex?))


(define g (gif (open-input-file "sample.gif")))

(define (global-color-quantity)
  (define val (dict-ref (dict-ref (dict-ref g 'logical-screen-descriptor) 'lsd-flags) 'global-color-table))
  (expt 2 (add1 val)))



g

#;(check-equal? (gif (gif (open-input-file "sample.gif"))) (read-bytes 13 (open-input-file "sample.gif")))

(require rackunit)
#;(check-equal? (parse-with-template "test.gif" gif)
                (cons 'gif
                      (make-hasheq (list (cons 'logical-screen-descriptor '(162 162 (#f #t #f #f #f #t #f #t) 0 0))
                                         '(signature . "GIF")
                                         '(version . "87a")))))
