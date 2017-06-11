#lang fontkit/racket
(require restructure)
(provide (all-defined-out))

(define 16bit-style 0)
(define 32bit-style 1)
(define max-32-bit-value #xffff)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/loca.js
|#

(define-subclass RVersionedStruct (Rloca)
  (define/override (process res stream)
    ;; in `restructure` `process` method, `res` is aliased as `this`
    ;; 
    (when (= 16bit-style (· res version))
      ;; in a 16bits-style loca table, actual 32bit offset values are divided by 2 (to fit into 16 bits)
      ;; so we re-inflate them.
      (hash-update! res 'offsets (λ (offsets) (map (curry * 2) offsets)))))

  (define/override (preEncode this-val stream)
    ;; this = val to be encoded
    (loca-preEncode this-val stream)))

;; make "static method"
(define (loca-preEncode this-val . args)
  ;; this = val to be encoded
  (unless (hash-has-key? this-val 'version)
    (hash-set! this-val 'version (if (> (last (· this-val offsets)) max-32-bit-value)
                                     32bit-style
                                     16bit-style))
    (when (= 16bit-style (· this-val version))
      (hash-update! this-val 'offsets (λ (offsets) (map (curryr / 2) offsets))))))

(define loca (make-object Rloca
               (λ (parent) (hash-ref (send parent _getTable 'head) 'indexToLocFormat)) 
               (dictify
                0 (dictify 'offsets (make-object RArray uint16be))
                1 (dictify 'offsets (make-object RArray uint32be))
                )))



