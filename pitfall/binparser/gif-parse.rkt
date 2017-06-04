#lang br
(require pitfall/binprint binparser racket/dict)

;; http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp

(define-rule gif-header (:seq [signature (:bytes 3 #:type string/ascii?)]
                              [version (:bytes 3 #:type string/ascii?)]
                              logical-screen-descriptor
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


(define ip (open-input-file "sample.gif"))

(define gh (gif-header ip))
gh

(define (global-color-quantity gh)
  (expt 2 (add1 (dict-ref* gh 'logical-screen-descriptor 'lsd-flags 'global-color-table))))

(define-rule color (:bytes 1 #:type hex?))
(define-rule red color)
(define-rule green color)
(define-rule blue color)
(define-rule global-color-table (:repeat (global-color-quantity gh) (:seq red green blue #:type assoc?)))
(define gct (global-color-table ip))

gct

(define-rule graphic-control-extension
  (:seq [extension-introducer (:bytes 1 #:type hex?)]
        [graphic-control-label (:bytes 1 #:type hex?)]
        [byte-size (:bytes 1 #:type integer?)]
        [gce-flags (:seq [transparent-color-flag (:bits 1 #:type boolean?)]
                         [user-input-flag (:bits 1 #:type boolean?)]
                         [disposal-method (:bits 3)]
                         [reserved (:bits 3)]
                         #:type assoc?)]
        [delay-time (:bytes 2 #:type integer?)]
        [transparent-color-idx (:bytes 1 #:type integer?)]
        [block-terminator (:bytes 1 #:type hex?)]
        #:type assoc?))

(graphic-control-extension ip)


(define-rule image-descriptor
  (:seq [image-separator (:bytes 1 #:type hex?)]
        [left (:bytes 2 #:type integer?)]
        [top (:bytes 2 #:type integer?)]
        [width (:bytes 2 #:type integer?)]
        [height (:bytes 2 #:type integer?)]
        [id-flags (:seq [local-color-table-size (:bits 3 #:type integer?)]
                         [reserved (:bits 2)]
                         [sort-flag (:bits 1)]
                         [interlace-flag (:bits 1)]
                         [local-color-table-flag (:bits 1)]
                         #:type assoc?)]
        #:type assoc?))

(image-descriptor ip)

#;(check-equal? (gif (gif (open-input-file "sample.gif"))) (read-bytes 13 (open-input-file "sample.gif")))

(require rackunit)
#;(check-equal? (parse-with-template "test.gif" gif)
                (cons 'gif
                      (make-hasheq (list (cons 'logical-screen-descriptor '(162 162 (#f #t #f #f #f #t #f #t) 0 0))
                                         '(signature . "GIF")
                                         '(version . "87a")))))
