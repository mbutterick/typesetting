#lang fontkit/racket
(require "directory.rkt")
(require rackunit "tables.rkt" restructure)



(define ip (open-input-file charter-path))
(define dir (directory-decode ip))

;; maxp 
(let ()
  (define maxp-offset (· dir tables maxp offset))
  (define maxp-length (· dir tables maxp length))
  (check-equal? maxp-offset 328)
  (check-equal? maxp-length 32)
  (define maxp-bytes #"\0\1\0\0\0\345\0f\0\a\0O\0\4\0\1\0\0\0\0\0\n\0\0\2\0\1s\0\2\0\1")
  (set-port-position! ip 0)
  (check-equal? (peek-bytes maxp-length maxp-offset ip) maxp-bytes)
  (define maxp-data (send maxp decode (make-object RDecodeStream maxp-bytes)))
  (check-equal? (· maxp-data numGlyphs) 229)
  (check-equal? (· maxp-data version) 65536))


;; hhea
(let ()
  (define offset (· dir tables hhea offset))
  (define length (· dir tables hhea length))
  (check-equal? offset 292)
  (check-equal? length 36)
  (define table-bytes #"\0\1\0\0\3\324\377\22\0\0\4\311\377_\377`\4\251\0\1\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\345")
  (set-port-position! ip 0)
  (check-equal? (peek-bytes length offset ip) table-bytes)
  (define table-data (send hhea decode (make-object RDecodeStream table-bytes)))
  (check-equal? (· table-data ascent) 980)
  (check-equal? (· table-data descent) -238))


;; head
(let ()
  (define ip (open-input-file charter-italic-path)) ; use italic to make sure style flags are set correctly
  (define dir (directory-decode ip))
  (define offset (· dir tables head offset))
  (define length (· dir tables head length))
  (check-equal? offset 236)
  (check-equal? length 54)
  (define table-bytes #"\0\1\0\0\0\2\0\0.\252t<_\17<\365\0\t\3\350\0\0\0\0\316\3\301\261\0\0\0\0\316\3\304\364\377\36\377\24\4\226\3\324\0\2\0\t\0\2\0\0\0\0")
  (set-port-position! ip 0)
  (check-equal? (peek-bytes length offset ip) table-bytes)
  (define table-data (send head decode (make-object RDecodeStream table-bytes)))
  (check-equal? (· table-data unitsPerEm) 1000)
  (check-equal? (· table-data yMin) -236)
  (check-equal? (· table-data yMax) 980)
  (check-equal? (· table-data xMax) 1174)
  (check-equal? (· table-data xMin) -226)
  (check-equal? (· table-data macStyle) (make-hash '((shadow . #f)
                                                     (extended . #f)
                                                     (condensed . #f)
                                                     (underline . #f)
                                                     (outline . #f)
                                                     (bold . #f)
                                                     (italic . #t))))
  (check-equal? (· table-data magicNumber) #x5F0F3CF5)
  (check-equal? (· table-data indexToLocFormat) 0) ; used in loca table
  )


;; prep
(let ()
  (define offset (· dir tables prep offset))
  (define len (· dir tables prep length))
  (check-equal? offset 4512)
  (check-equal? len 78)
  (set-port-position! ip 0)
  (define table-bytes #"\270\0\0+\0\272\0\1\0\1\0\2+\1\272\0\2\0\1\0\2+\1\277\0\2\0C\0007\0+\0\37\0\23\0\0\0\b+\0\277\0\1\0\200\0i\0R\0;\0#\0\0\0\b+\0\272\0\3\0\5\0\a+\270\0\0 E}i\30D")
  (check-equal? table-bytes (peek-bytes len offset ip))
  (define ds (make-object RDecodeStream (peek-bytes len offset ip)))
  (check-equal? (hash-ref (send prep decode ds) 'controlValueProgram) '(184
                                                                        0
                                                                        0
                                                                        43
                                                                        0
                                                                        186
                                                                        0
                                                                        1
                                                                        0
                                                                        1
                                                                        0
                                                                        2
                                                                        43
                                                                        1
                                                                        186
                                                                        0
                                                                        2
                                                                        0
                                                                        1
                                                                        0
                                                                        2
                                                                        43
                                                                        1
                                                                        191
                                                                        0
                                                                        2
                                                                        0
                                                                        67
                                                                        0
                                                                        55
                                                                        0
                                                                        43
                                                                        0
                                                                        31
                                                                        0
                                                                        19
                                                                        0
                                                                        0
                                                                        0
                                                                        8
                                                                        43
                                                                        0
                                                                        191
                                                                        0
                                                                        1
                                                                        0
                                                                        128
                                                                        0
                                                                        105
                                                                        0
                                                                        82
                                                                        0
                                                                        59
                                                                        0
                                                                        35
                                                                        0
                                                                        0
                                                                        0
                                                                        8
                                                                        43
                                                                        0
                                                                        186
                                                                        0
                                                                        3
                                                                        0
                                                                        5
                                                                        0
                                                                        7
                                                                        43
                                                                        184
                                                                        0
                                                                        0
                                                                        32
                                                                        69
                                                                        125
                                                                        105
                                                                        24
                                                                        68)))

;; fpgm
(let ()
  (define offset (· dir tables fpgm offset))
  (define len (· dir tables fpgm length))
  (check-equal? offset 4140)
  (check-equal? len 371)
  (set-port-position! ip 0)
  (define ds (make-object RDecodeStream (peek-bytes len offset ip)))
  (check-equal? (hash-ref (send fpgm decode ds) 'instructions) '(184
                                                                 0
                                                                 0
                                                                 44
                                                                 75
                                                                 184
                                                                 0
                                                                 9
                                                                 80
                                                                 88
                                                                 177
                                                                 1
                                                                 1
                                                                 142
                                                                 89
                                                                 184
                                                                 1
                                                                 255
                                                                 133
                                                                 184
                                                                 0
                                                                 68
                                                                 29
                                                                 185
                                                                 0
                                                                 9
                                                                 0
                                                                 3
                                                                 95
                                                                 94
                                                                 45
                                                                 184
                                                                 0
                                                                 1
                                                                 44
                                                                 32
                                                                 32
                                                                 69
                                                                 105
                                                                 68
                                                                 176
                                                                 1
                                                                 96
                                                                 45
                                                                 184
                                                                 0
                                                                 2
                                                                 44
                                                                 184
                                                                 0
                                                                 1
                                                                 42
                                                                 33
                                                                 45
                                                                 184
                                                                 0
                                                                 3
                                                                 44
                                                                 32
                                                                 70
                                                                 176
                                                                 3
                                                                 37
                                                                 70
                                                                 82
                                                                 88
                                                                 35
                                                                 89
                                                                 32
                                                                 138
                                                                 32
                                                                 138
                                                                 73
                                                                 100
                                                                 138
                                                                 32
                                                                 70
                                                                 32
                                                                 104
                                                                 97
                                                                 100
                                                                 176
                                                                 4
                                                                 37
                                                                 70
                                                                 32
                                                                 104
                                                                 97
                                                                 100
                                                                 82
                                                                 88
                                                                 35
                                                                 101
                                                                 138
                                                                 89
                                                                 47
                                                                 32
                                                                 176
                                                                 0
                                                                 83
                                                                 88
                                                                 105
                                                                 32
                                                                 176
                                                                 0
                                                                 84
                                                                 88
                                                                 33
                                                                 176
                                                                 64
                                                                 89
                                                                 27
                                                                 105
                                                                 32
                                                                 176
                                                                 0
                                                                 84
                                                                 88
                                                                 33
                                                                 176
                                                                 64
                                                                 101
                                                                 89
                                                                 89
                                                                 58
                                                                 45
                                                                 184
                                                                 0
                                                                 4
                                                                 44
                                                                 32
                                                                 70
                                                                 176
                                                                 4
                                                                 37
                                                                 70
                                                                 82
                                                                 88
                                                                 35
                                                                 138
                                                                 89
                                                                 32
                                                                 70
                                                                 32
                                                                 106
                                                                 97
                                                                 100
                                                                 176
                                                                 4
                                                                 37
                                                                 70
                                                                 32
                                                                 106
                                                                 97
                                                                 100
                                                                 82
                                                                 88
                                                                 35
                                                                 138
                                                                 89
                                                                 47
                                                                 253
                                                                 45
                                                                 184
                                                                 0
                                                                 5
                                                                 44
                                                                 75
                                                                 32
                                                                 176
                                                                 3
                                                                 38
                                                                 80
                                                                 88
                                                                 81
                                                                 88
                                                                 176
                                                                 128
                                                                 68
                                                                 27
                                                                 176
                                                                 64
                                                                 68
                                                                 89
                                                                 27
                                                                 33
                                                                 33
                                                                 32
                                                                 69
                                                                 176
                                                                 192
                                                                 80
                                                                 88
                                                                 176
                                                                 192
                                                                 68
                                                                 27
                                                                 33
                                                                 89
                                                                 89
                                                                 45
                                                                 184
                                                                 0
                                                                 6
                                                                 44
                                                                 32
                                                                 32
                                                                 69
                                                                 105
                                                                 68
                                                                 176
                                                                 1
                                                                 96
                                                                 32
                                                                 32
                                                                 69
                                                                 125
                                                                 105
                                                                 24
                                                                 68
                                                                 176
                                                                 1
                                                                 96
                                                                 45
                                                                 184
                                                                 0
                                                                 7
                                                                 44
                                                                 184
                                                                 0
                                                                 6
                                                                 42
                                                                 45
                                                                 184
                                                                 0
                                                                 8
                                                                 44
                                                                 75
                                                                 32
                                                                 176
                                                                 3
                                                                 38
                                                                 83
                                                                 88
                                                                 176
                                                                 64
                                                                 27
                                                                 176
                                                                 0
                                                                 89
                                                                 138
                                                                 138
                                                                 32
                                                                 176
                                                                 3
                                                                 38
                                                                 83
                                                                 88
                                                                 35
                                                                 33
                                                                 176
                                                                 128
                                                                 138
                                                                 138
                                                                 27
                                                                 138
                                                                 35
                                                                 89
                                                                 32
                                                                 176
                                                                 3
                                                                 38
                                                                 83
                                                                 88
                                                                 35
                                                                 33
                                                                 184
                                                                 0
                                                                 192
                                                                 138
                                                                 138
                                                                 27
                                                                 138
                                                                 35
                                                                 89
                                                                 32
                                                                 176
                                                                 3
                                                                 38
                                                                 83
                                                                 88
                                                                 35
                                                                 33
                                                                 184
                                                                 1
                                                                 0
                                                                 138
                                                                 138
                                                                 27
                                                                 138
                                                                 35
                                                                 89
                                                                 32
                                                                 176
                                                                 3
                                                                 38
                                                                 83
                                                                 88
                                                                 35
                                                                 33
                                                                 184
                                                                 1
                                                                 64
                                                                 138
                                                                 138
                                                                 27
                                                                 138
                                                                 35
                                                                 89
                                                                 32
                                                                 184
                                                                 0
                                                                 3
                                                                 38
                                                                 83
                                                                 88
                                                                 176
                                                                 3
                                                                 37
                                                                 69
                                                                 184
                                                                 1
                                                                 128
                                                                 80
                                                                 88
                                                                 35
                                                                 33
                                                                 184
                                                                 1
                                                                 128
                                                                 35
                                                                 33
                                                                 27
                                                                 176
                                                                 3
                                                                 37
                                                                 69
                                                                 35
                                                                 33
                                                                 35
                                                                 33
                                                                 89
                                                                 27
                                                                 33
                                                                 89
                                                                 68
                                                                 45
                                                                 184
                                                                 0
                                                                 9
                                                                 44
                                                                 75
                                                                 83
                                                                 88
                                                                 69
                                                                 68
                                                                 27
                                                                 33
                                                                 33
                                                                 89
                                                                 45)))

;; loca
(let ()
  (define offset (· dir tables loca offset))
  (define len (· dir tables loca length))
  (check-equal? offset 38692)
  (check-equal? len 460)
  (set-port-position! ip 0)
  (define ds (make-object RDecodeStream (peek-bytes len offset ip)))
  (define table-data (send loca decode ds #:version 0))
  (check-equal? (length (· table-data offsets)) 230)
  (check-equal? (· table-data offsets) '(0
                                         0
                                         0
                                         136
                                         296
                                         500
                                         864
                                         1168
                                         1548
                                         1628
                                         1716
                                         1804
                                         1944
                                         2048
                                         2128
                                         2176
                                         2256
                                         2312
                                         2500
                                         2596
                                         2788
                                         3052
                                         3168
                                         3396
                                         3624
                                         3732
                                         4056
                                         4268
                                         4424
                                         4564
                                         4640
                                         4728
                                         4804
                                         5012
                                         5384
                                         5532
                                         5808
                                         6012
                                         6212
                                         6456
                                         6672
                                         6916
                                         7204
                                         7336
                                         7496
                                         7740
                                         7892
                                         8180
                                         8432
                                         8648
                                         8892
                                         9160
                                         9496
                                         9764
                                         9936
                                         10160
                                         10312
                                         10536
                                         10780
                                         10992
                                         11148
                                         11216
                                         11272
                                         11340
                                         11404
                                         11444
                                         11524
                                         11820
                                         12044
                                         12216
                                         12488
                                         12728
                                         12932
                                         13324
                                         13584
                                         13748
                                         13924
                                         14128
                                         14232
                                         14592
                                         14852
                                         15044
                                         15336
                                         15588
                                         15776
                                         16020
                                         16164
                                         16368
                                         16520
                                         16744
                                         16984
                                         17164
                                         17320
                                         17532
                                         17576
                                         17788
                                         17896
                                         18036
                                         18284
                                         18552
                                         18616
                                         18988
                                         19228
                                         19512
                                         19712
                                         19796
                                         19976
                                         20096
                                         20160
                                         20224
                                         20536
                                         20836
                                         20876
                                         21000
                                         21200
                                         21268
                                         21368
                                         21452
                                         21532
                                         21720
                                         21908
                                         22036
                                         22244
                                         22664
                                         22872
                                         22932
                                         22992
                                         23088
                                         23220
                                         23268
                                         23372
                                         23440
                                         23600
                                         23752
                                         23868
                                         23988
                                         24084
                                         24184
                                         24224
                                         24548
                                         24788
                                         25012
                                         25292
                                         25716
                                         25884
                                         26292
                                         26396
                                         26540
                                         26796
                                         27172
                                         27488
                                         27512
                                         27536
                                         27560
                                         27584
                                         27912
                                         27936
                                         27960
                                         27984
                                         28008
                                         28032
                                         28056
                                         28080
                                         28104
                                         28128
                                         28152
                                         28176
                                         28200
                                         28224
                                         28248
                                         28272
                                         28296
                                         28320
                                         28344
                                         28368
                                         28392
                                         28416
                                         28440
                                         28464
                                         28488
                                         28512
                                         28536
                                         28560
                                         28968
                                         28992
                                         29016
                                         29040
                                         29064
                                         29088
                                         29112
                                         29136
                                         29160
                                         29184
                                         29208
                                         29232
                                         29256
                                         29280
                                         29304
                                         29328
                                         29352
                                         29376
                                         29400
                                         29424
                                         29448
                                         29472
                                         29496
                                         29520
                                         29824
                                         30164
                                         30220
                                         30652
                                         30700
                                         30956
                                         31224
                                         31248
                                         31332
                                         31488
                                         31636
                                         31916
                                         32104
                                         32176
                                         32484
                                         32744
                                         32832
                                         32956
                                         33248
                                         33664
                                         33884
                                         34048
                                         34072)))
