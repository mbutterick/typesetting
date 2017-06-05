#lang br
(require pitfall/binprint binparser racket/dict)

;; http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp

(define ip (open-input-file "sample.gif"))


(define-rule gif-header (:seq [signature (:bytes 3 #:type string/ascii?)]
                              [version (:bytes 3 #:type string/ascii?)]
                              logical-screen-descriptor
                              #:type assoc?))

(define-rule logical-screen-descriptor (:seq [width (:bytes 2 #:type integer?)]
                                             [height (:bytes 2 #:type integer?)]
                                             [lsd-flags (:bitfield [global-color-table-size (:bits 3 #:type integer?)]
                                                                   [sort (:bits 1 #:type boolean?)]
                                                                   [resolution (:bits 3 #:type integer?)]
                                                                   [global-color-table (:bits 1 #:type integer?)]
                                                                   #:type assoc?)]
                                             [bgcolor-idx (:bytes 1 #:type integer?)]
                                             [aspect (:bytes 1 #:type integer?)]
                                             #:type assoc?))



(define gh (gif-header ip))
gh

(define (color-quantity table-size)
  (expt 2 (add1 table-size)))



(define-rule hex-color (:bytes 1 #:type hex?))
(define-rule red hex-color)
(define-rule green hex-color)
(define-rule blue hex-color)
(define-rule color (:seq red green blue))
(define (global-color-quantity gh)
  (color-quantity (dict-ref* gh 'logical-screen-descriptor 'lsd-flags 'global-color-table)))
(define-rule global-color-table (:repeat (global-color-quantity gh) color #:type assoc?))
(define gct (global-color-table ip))

gct

(define-rule graphic-control-extension
  (:seq [extension-introducer (:bytes 1 #:type hex?)]
        [graphic-control-label (:bytes 1 #:type hex?)]
        [byte-size (:bytes 1 #:type integer?)]
        [gce-flags (:bitfield [transparent-color-flag (:bits 1 #:type boolean?)]
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
                        [local-color-table-flag (:bits 1 #:type integer?)]
                        #:type assoc?)]
        #:type assoc?))

(define img-descriptor (image-descriptor ip))

(define (local-color-quantity gh)
  (* (dict-ref* img-descriptor 'id-flags 'local-color-table-flag)
     (color-quantity (dict-ref* img-descriptor 'id-flags 'local-color-table-size))))
(define-rule local-color-table (:repeat (local-color-quantity gh) color #:type assoc?))
(define lct (local-color-table ip))

lct

(define-rule lzw-minimum-code-size (:bytes 1 #:type integer?))

(lzw-minimum-code-size ip)

(for/list ([block-len (in-port read-byte ip)]
           #:break (zero? block-len))
  ((:bytes block-len #:type hex?) ip))

(define-rule trailer (:bytes 1 #:type hex?))

(trailer ip)