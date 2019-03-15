#lang debug racket/base
(require racket/class racket/match racket/list xenomorph
         "cff-top.rkt"
         "cff-standard-strings.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFFont.js
|#

;; the CFFFont object acts as the decoder for the `CFF ` table.
;; so it should return a hash.

(define CFFFont%
  (class x:base%
    (super-new)

    (define/augride (x:decode stream parent . _)
      (define cff-font (make-hasheq))
      (hash-set! cff-font 'stream stream)
          
      (for ([(k v) (in-hash (decode CFFTop stream))])
           (hash-set! cff-font k v))
          
      ;; because fontkit depends on overloading 'version key, and we don't
      (hash-set! cff-font 'version (hash-ref cff-font 'x:version))

      (when (and (hash-has-key? cff-font 'version) (< (hash-ref cff-font 'version) 2))
        (match (hash-ref cff-font 'topDictIndex)
          [(vector dict) (hash-set! cff-font 'topDict dict)]
          [_ (error 'only-single-font-allowed-in-cff)]))

      (hash-set! cff-font 'isCIDFont (hash-ref (hash-ref cff-font 'topDict) 'ROS))
      cff-font)))

(define (CFFont-string this sid)
  (cond
    [(not sid) #false]
    [(>= (hash-ref this 'version) 2) #false]
    [(< sid (vector-length standardStrings)) (vector-ref standardStrings sid)]
    [else (vector-ref (hash-ref this 'stringIndex) (- sid (vector-length standardStrings)))]))

(define (CFFFont-postscriptName this)
  (and (< (hash-ref this 'version) 2) (car (hash-ref this 'nameIndex))))

(define CFFFont (make-object CFFFont%))

(define (getCharString cff-font glyph-id)
  (define glyph-record (vector-ref (hash-ref (hash-ref cff-font 'topDict) 'CharStrings) glyph-id))
  (pos (hash-ref cff-font 'stream) (hash-ref glyph-record 'offset))
  (read-bytes (hash-ref glyph-record 'length) (hash-ref cff-font 'stream)))

(define (fdForGlyph this gid)
  (cond
    [(not (hash-has-key? (hash-ref this 'topDict) 'FDSelect)) #false]
    [else
     (match (hash-ref* this 'topDict 'FDSelect 'version)
       [0 (list-ref (hash-ref* this 'topDict 'FDSelect) gid)]
       [(or 3 4)
        (define ranges (hash-ref* this 'topDict 'FDSelect 'ranges))
        (let loop ([low 0][high (sub1 (length ranges))])
          (when (<= low high)
            (define mid (arithmetic-shift (+ low high) -1))
            (cond
              [(< gid (hash-ref (list-ref ranges mid) 'first))
               (loop low (sub1 mid))]
              [(and (< mid high) (> gid (hash-ref (list-ref ranges (add1 mid)) 'first)))
               (loop (add1 mid) high)]
              [else (hash-ref (list-ref ranges mid) 'fd)])))]
       [default (error 'unknown-select-version)])]))

(define (privateDictForGlyph this gid)
  (cond
    [(and (hash-has-key? this 'topDict)
          (hash-has-key? (hash-ref this 'topDict) 'FDSelect)
          (hash-ref* this 'topDict 'FDSelect))
     (define fd (fdForGlyph this gid))
     (if (list-ref (hash-ref* this 'topDict 'FDArray) fd)
         (hash-ref (list-ref (hash-ref* 'topDict 'FDArray) fd) 'Private)
         #false)]
    [(< (hash-ref this 'version) 2) (hash-ref* this 'topDict 'Private)]
    [else (hash-ref (list-ref (hash-ref* this 'topDict 'FDArray) 0) 'Private)]))

(module+ test
  (require rackunit racket/serialize racket/stream fontland/helper)
  (define dir (deserialize (read (open-input-file fira-otf-directory-path))))
  (define cff (hash-ref (hash-ref dir 'tables) 'CFF_))
  (check-equal? (hash-ref cff 'length) 164604)
  (define ip (open-input-file fira-otf-path))
  (define cff-offset (hash-ref cff 'offset))
  (check-equal? cff-offset 33472)
  (file-position ip cff-offset)
  (define cff-font (decode CFFFont ip))
  (check-equal? (file-position (hash-ref cff-font 'stream)) 74651)
  (check-equal? (hash-ref cff-font 'version) 1)
  (check-equal? (hash-ref cff-font 'hdrSize) 4)
  (check-equal? (hash-ref cff-font 'offSize) 3)
  (check-equal? (hash-ref cff-font 'nameIndex) '("FiraSans-Book"))
  (check-equal? (length (hash-ref cff-font 'globalSubrIndex)) 820)
  (check-equal?
   (for/list ([h (in-list (hash-ref cff-font 'globalSubrIndex))])
             (hash-ref h 'offset))
   '(60105 60130 60218 60264 60303 60330 60361 60366 60387 60427 60433 60447 60454 60469 60500 60506 60512 60516 60545 60566 60581 60624 60637 60667 60679 60705 60715 60755 60776 60781 60839 60891 60897 60907 60914 60920 60938 60950 60976 60992 61005 61011 61032 61051 61067 61097 61111 61172 61272 61284 61359 61430 61489 61522 61526 61531 61535 61543 61565 61570 61575 61579 61601 61615 61629 61649 61654 61664 61842 61849 61858 61865 61895 61913 61920 61964 61977 61996 62074 62094 62102 62128 62132 62149 62160 62170 62197 62216 62225 62230 62237 62247 62256 62285 62332 62339 62347 62350 62375 62435 62479 62511 62539 62561 62585 62605 62621 62632 62711 62717 62733 62743 62783 62809 62818 62868 62905 62955 62965 62971 63034 63050 63059 63191 63237 63358 63394 63460 63465 63592 63716 63740 63866 63924 63947 64051 64075 64099 64120 64184 64245 64260 64374 64493 64515 64543 64585 64592 64597 64611 64622 64735 64738 64789 64797 64882 64920 65027 65054 65057 65069 65077 65113 65125 65222 65254 65275 65377 65480 65516 65524 65530 65550 65565 65569 65576 65673 65691 65760 65836 65854 65866 65873 65881 65895 65924 65929 65949 65970 66060 66093 66113 66132 66146 66151 66160 66165 66174 66185 66192 66210 66231 66255 66280 66288 66296 66301 66386 66395 66400 66446 66455 66537 66545 66550 66555 66636 66712 66722 66729 66748 66774 66788 66797 66810 66818 66841 66847 66853 66872 66877 66882 66887 66962 66988 66997 67008 67021 67027 67034 67040 67047 67110 67180 67218 67256 67325 67355 67369 67376 67390 67399 67403 67471 67478 67499 67520 67524 67550 67565 67579 67584 67651 67671 67679 67684 67749 67759 67772 67783 67790 67817 67883 67944 67967 67986 68049 68056 68090 68113 68132 68139 68149 68154 68159 68222 68226 68259 68262 68323 68326 68335 68372 68413 68420 68427 68435 68441 68446 68451 68462 68477 68489 68530 68535 68548 68553 68560 68567 68622 68638 68694 68748 68759 68764 68816 68862 68880 68885 68900 68907 68959 68988 69002 69011 69016 69028 69037 69089 69099 69115 69131 69143 69152 69160 69168 69174 69180 69202 69213 69218 69268 69318 69325 69374 69383 69402 69415 69422 69427 69434 69446 69488 69514 69529 69535 69582 69587 69603 69647 69667 69678 69684 69690 69700 69705 69710 69752 69795 69816 69860 69888 69898 69912 69921 69932 69936 69943 69948 69991 70002 70028 70041 70051 70057 70099 70130 70151 70166 70207 70219 70257 70279 70290 70300 70309 70316 70325 70333 70341 70346 70352 70357 70364 70401 70438 70475 70480 70487 70491 70497 70502 70532 70545 70552 70557 70562 70599 70616 70647 70651 70658 70665 70670 70706 70712 70737 70754 70766 70778 70786 70798 70804 70812 70817 70823 70858 70893 70904 70908 70913 70926 70933 70947 70954 70962 70968 70976 70981 70987 70996 71002 71007 71012 71018 71041 71074 71079 71111 71143 71175 71192 71196 71207 71215 71220 71226 71234 71247 71254 71261 71291 71308 71314 71322 71339 71345 71350 71354 71379 71395 71404 71413 71417 71422 71436 71458 71463 71479 71491 71501 71509 71521 71528 71535 71541 71547 71555 71561 71568 71575 71582 71588 71594 71599 71626 71640 71666 71692 71696 71704 71722 71735 71750 71759 71766 71781 71792 71797 71802 71808 71813 71818 71840 71865 71890 71894 71905 71910 71916 71927 71935 71942 71949 71956 71961 71966 71977 72001 72014 72036 72044 72056 72070 72076 72082 72087 72093 72100 72112 72135 72151 72155 72163 72172 72177 72182 72195 72217 72239 72261 72277 72297 72304 72324 72337 72350 72355 72365 72370 72377 72387 72394 72398 72405 72412 72417 72423 72429 72446 72460 72465 72485 72495 72514 72523 72530 72535 72546 72566 72586 72604 72624 72642 72651 72660 72666 72678 72684 72689 72698 72707 72714 72722 72730 72735 72740 72746 72752 72758 72774 72793 72799 72812 72825 72834 72839 72858 72862 72868 72877 72886 72895 72904 72913 72922 72931 72940 72946 72953 72960 72967 72973 72978 72985 73000 73007 73025 73043 73048 73066 73073 73089 73100 73111 73118 73124 73135 73140 73146 73152 73158 73163 73168 73173 73186 73200 73217 73234 73249 73264 73279 73286 73302 73318 73332 73348 73358 73374 73384 73390 73395 73403 73413 73422 73428 73438 73445 73452 73459 73465 73473 73479 73486 73491 73497 73505 73513 73521 73528 73533 73538 73545 73552 73559 73566 73572 73587 73592 73604 73619 73634 73649 73664 73679 73694 73709 73722 73735 73743 73748 73753 73758 73771 73784 73798 73804 73814 73827 73839 73843 73851 73860 73865 73872 73881 73890 73899 73908 73914 73920 73926 73932 73938 73943 73948 73953 73963 73976 73989 74002 74015 74028 74037 74050 74063 74076 74087 74098 74104 74110 74117 74123 74130 74137 74144 74149 74156 74161 74166 74171 74176 74188 74200 74212 74224 74236 74248 74260 74266 74278 74282 74294 74306 74313 74321 74328 74336 74342 74350 74357 74364 74370 74377 74385 74390 74396 74402 74408 74414 74420 74425 74430 74435 74440 74445 74450 74455 74466 74477 74488 74498 74509 74520 74531 74542 74553 74564 74575 74584 74593 74602 74611 74616 74621 74626 74631 74636 74641 74646))
  (check-equal? (length (hash-ref cff-font 'stringIndex)) 2404)
  #;(check-equal? (hash-ref (hash-ref cff-font 'topDict) 'version) 2401)
  (check-equal?
   (list-ref (hash-ref cff-font 'stringIndex) 2401)
   "004.106")
  (check-equal?
   (list-ref (hash-ref cff-font 'stringIndex) 2402)
   "Digitized data copyright \\(c\\) 2012-2015, The Mozilla Foundation and Telefonica S.A.")
  (check-equal?
   (list-ref (hash-ref cff-font 'stringIndex) 2403)
   "Fira Sans Book")

  (define top-dict (hash-ref cff-font 'topDict))
  (check-equal? (hash-ref top-dict 'FontBBox) '(-167 -350 1360 1093))
  (check-equal? (hash-ref top-dict 'version) 2792)
  (check-equal? (hash-ref top-dict 'Notice) 2793)
  (check-equal? (hash-ref top-dict 'FullName) 2794)
  (check-equal? (hash-ref top-dict 'Weight) 388)

  (define private (hash-ref top-dict 'Private))
  (check-equal? (hash-ref private 'StdHW) 68)
  (check-equal? (hash-ref private 'StdVW) 84)
  (check-equal? (hash-ref private 'defaultWidthX) 0)
  (check-equal? (hash-ref private 'nominalWidthX) 553)
  (check-equal? (hash-ref private 'BlueScale) 0.037)
  (check-equal? (hash-ref private 'BlueShift) 7)
  (check-equal? (hash-ref private 'ExpansionFactor) 0.06)
  (check-equal?
   (for/list ([h (in-list (take (hash-ref top-dict 'CharStrings) 100))])
             (hash-ref h 'offset))
   '(83610 83750 83753 83755 83776 83778 83810 83858 83890 83951 84023 84046 84068 84096 84132 84169 84233 84270 84292 84322 84380 84411 84439 84478 84498 84547 84575 84679 84711 84751 84784 84823 84919 84956 84964 84978 85011 85013 85101 85188 85300 85302 85396 85398 85407 85422 85436 85451 85547 85561 85587 85647 85784 85790 85824 85864 85933 85935 85960 85970 85972 86003 86027 86091 86106 86161 86176 86228 86238 86253 86273 86288 86347 86363 86385 86401 86423 86463 86496 86511 86541 86568 86578 86594 86627 86651 86680 86731 86733 86766 86769 86861 86887 86900 86919 86986 87017 87061 87098 87108))
  )