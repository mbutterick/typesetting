#lang racket/base
(require xenomorph
         sugar/unstable/class
         sugar/unstable/js
         sugar/unstable/dict
         racket/class
         racket/list
         "../helper.rkt")
(provide (all-defined-out))

(define 16bit-style 0)
(define 32bit-style 1)
(define max-32-bit-value #xffff)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/loca.js
|#

(define-subclass VersionedStruct (Rloca)
  (define/augride (post-decode res stream ctx)
    ;; in `xenomorph` `process` method, `res` is aliased as `this`
    ;;
    (when (= 16bit-style (· res version))
      ;; in a 16bits-style loca table, actual 32bit offset values are divided by 2 (to fit into 16 bits)
      ;; so we re-inflate them.
      (dict-update! res 'offsets (λ (offsets) (map (λ (x) (* 2 x)) offsets))))
    res)

  (define/augride (pre-encode this-val stream)
    ;; this = val to be encoded
    (loca-pre-encode this-val stream)
    this-val))

;; make "static method"
(define (loca-pre-encode this . args)
  ;; this = val to be encoded
  (unless (dict-has-key? this 'version)
    (dict-set! this 'version (if (> (last (· this offsets)) max-32-bit-value)
                                 32bit-style
                                 16bit-style))
    (when (= 16bit-style (· this version))
      (dict-update! this 'offsets (λ (offsets) (map (λ (x) (/ x 2)) offsets))))))

(define loca (+Rloca
              (λ (o) (· o head indexToLocFormat)) 
              (dictify
               0 (dictify 'offsets (+Array uint16be))
               1 (dictify 'offsets (+Array uint32be)))))


(test-module
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define offset (· dir tables loca offset))
 (define len (· dir tables loca length))
 (check-equal? offset 38692)
 (check-equal? len 460)
 (define ds (peek-bytes len offset ip))
 (check-equal?
  (send loca encode #f (mhash 'version 0 'offsets '(0 76 156))) #"\0\0\0L\0\234")
 (check-equal?
  (send loca encode #f '#hash((version . 1) (offsets . (0 76 156)))) #"\0\0\0\0\0\0\0L\0\0\0\234")
 (send loca force-version! 0)
 (define table-data (send loca decode ds))
 (check-equal? (length (· table-data offsets)) 230)
 (check-equal? (· table-data offsets) '(0 0 0 136 296 500 864 1168 1548 1628 1716 1804 1944 2048 2128 2176 2256 2312 2500 2596 2788 3052 3168 3396 3624 3732 4056 4268 4424 4564 4640 4728 4804 5012 5384 5532 5808 6012 6212 6456 6672 6916 7204 7336 7496 7740 7892 8180 8432 8648 8892 9160 9496 9764 9936 10160 10312 10536 10780 10992 11148 11216 11272 11340 11404 11444 11524 11820 12044 12216 12488 12728 12932 13324 13584 13748 13924 14128 14232 14592 14852 15044 15336 15588 15776 16020 16164 16368 16520 16744 16984 17164 17320 17532 17576 17788 17896 18036 18284 18552 18616 18988 19228 19512 19712 19796 19976 20096 20160 20224 20536 20836 20876 21000 21200 21268 21368 21452 21532 21720 21908 22036 22244 22664 22872 22932 22992 23088 23220 23268 23372 23440 23600 23752 23868 23988 24084 24184 24224 24548 24788 25012 25292 25716 25884 26292 26396 26540 26796 27172 27488 27512 27536 27560 27584 27912 27936 27960 27984 28008 28032 28056 28080 28104 28128 28152 28176 28200 28224 28248 28272 28296 28320 28344 28368 28392 28416 28440 28464 28488 28512 28536 28560 28968 28992 29016 29040 29064 29088 29112 29136 29160 29184 29208 29232 29256 29280 29304 29328 29352 29376 29400 29424 29448 29472 29496 29520 29824 30164 30220 30652 30700 30956 31224 31248 31332 31488 31636 31916 32104 32176 32484 32744 32832 32956 33248 33664 33884 34048 34072))
 )
