#lang debug racket/base
(require racket/serialize
         racket/list
         racket/match
         sugar/unstable/dict
         "table/loca.rkt"
         "table-stream.rkt"
         "directory.rkt"
         "struct.rkt"
         fontland/glyph
         fontland/ttf-glyph
         fontland/cff-glyph
         xenomorph
         racket/dict
         fontland/table/cff/cff-font
         fontland/table/cff/cff-top
         fontland/table/cff/cff-standard-strings)

(provide subset +subset
         ttf-subset +ttf-subset ttf-subset?
         cff-subset +cff-subset cff-subset?
         subset-add-glyph! encode-to-port create-subset)

#|
from
https://github.com/mbutterick/fontkit/blob/master/src/TTFFont.js
|#

(define (create-subset font)
  ((if (has-table? font #"CFF_")
       +cff-subset
       +ttf-subset) font))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/subset/Subset.js
|#

(define (+subset font [glyphs empty] [mapping (mhash)])
  (define ss (subset font glyphs mapping))
  (subset-add-glyph! ss 0)
  ss)

(define (encode-to-port ss)
  (define p (open-output-bytes))
  ((if (cff-subset? ss)
       cff-subset-encode
       ttf-subset-encode) ss p)
  p)

(define (subset-add-glyph! ss glyph-or-gid) ; fka `includeGlyph`
  (define new-gid ((if (glyph? glyph-or-gid) glyph-id values) glyph-or-gid))
  ;; put the new glyph at the end of `glyphs`,
  ;; and put its index in the mapping
  (hash-ref! (subset-mapping ss) new-gid
             (λ ()
               (set-subset-glyphs! ss (append (subset-glyphs ss) (list new-gid)))
               (sub1 (length (subset-glyphs ss))))))  


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/subset/CFFSubset.js
|#


(define (+cff-subset font [glyphs empty] [mapping (mhash)]
                     [cff (get-table font 'CFF_)]
                     [strings #f]
                     [charstrings #f]
                     [gsubrs #f])
  (define ss (cff-subset font glyphs mapping cff strings charstrings gsubrs))
  (subset-add-glyph! ss 0)
  ss)

(require racket/format racket/string)
(define (bytes->hexes bs)
  (string-join
   (for/list ([b (in-bytes bs)])
     (~r #:base 16 b #:min-width 2 #:pad-string "0"))  " "))

(define (subset-charstrings this)
  (set-cff-subset-charstrings! this null)
  (define gsubrs (make-hasheq))
  (for ([gid (in-list (subset-glyphs this))])
    (set-cff-subset-charstrings!
     this
     (append (cff-subset-charstrings this)
             (list (get-char-string (cff-subset-cff this) gid))))

    (define glyph (get-glyph (subset-font this) gid))
    (unless (cff-glyph-path glyph) (getPath glyph)) ;; this causes the glyph to be parsed

    (for ([subr (in-hash-keys (cff-glyph-_usedGsubrs glyph))])
      (hash-set! gsubrs subr #true)))

  (set-cff-subset-gsubrs! this (subset-subrs
                                this
                                (hash-ref (cff-subset-cff this) 'globalSubrIndex)
                                gsubrs)))

(define (subset-subrs this subrs used)
  (for/vector ([(subr i) (in-indexed subrs)])
    (cond
      [(hash-ref used i #false)
       (pos (hash-ref (cff-subset-cff this) 'stream) (index-item-offset subr))
       (read-bytes (index-item-length subr) (hash-ref (cff-subset-cff this) 'stream))]
      [else (bytes 11)])))


(define (subset-font-dict this topDict)
  (error 'subsetFontdict-unimplemented))


(define (create-cid-fontdict this top-dict)
  (define used-subrs (make-hasheq))
  (for ([gid (in-list (subset-glyphs this))])
    (define glyph (get-glyph (subset-font this) gid))
    (unless (cff-glyph-path glyph) (getPath glyph)) ;; this causes the glyph to be parsed
       
    (for ([subr (in-hash-keys (cff-glyph-_usedSubrs glyph))])
      (hash-set! used-subrs subr #true)))

  (define cff-topDict (hash-ref (cff-subset-cff this) 'topDict))
  (define private-dict (hash-copy (hash-ref cff-topDict 'Private (make-hasheq))))
  (when (and (hash-has-key? cff-topDict 'Private) (hash-has-key? (hash-ref cff-topDict 'Private) 'Subrs))
    (hash-set! private-dict 'Subrs (subset-subrs this
                                                 (hash-ref (hash-ref cff-topDict 'Private) 'Subrs)
                                                 used-subrs)))

  (hash-set! top-dict 'FDArray (list (dictify 'Private private-dict)))
  (hash-set! top-dict 'FDSelect (dictify 'version 3
                                         'nRanges 1
                                         'ranges (list (dictify 'first 0 'fd 0))
                                         'sentinel (length (cff-subset-charstrings this))))
  (hash-ref top-dict 'FDSelect))
           
(define (add-string this [string #f])
  (cond
    [(not string) #false]
    [else
     (unless (cff-subset-strings this)
       (set-cff-subset-strings! this null))

     (set-cff-subset-strings! this
                              (append (cff-subset-strings this) (list string)))
     (+ (vector-length standard-strings) (sub1 (length (cff-subset-strings this))))]))
   

(define (cff-subset-encode this stream)
  (subset-charstrings this)

  (define charset
    (dictify 'version (if (> (length (cff-subset-charstrings this)) 255) 2 1)
             'ranges (list (dictify 'first 1 'nLeft (- (length (cff-subset-charstrings this)) 2)))))
  
  (define top-dict (hash-copy (hash-ref (cff-subset-cff this) 'topDict)))
  (hash-set*! top-dict
              'Private #false
              'charset charset
              'Encoding #false
              'CharStrings (cff-subset-charstrings this))
  
  (for ([key (in-list '(version Notice Copyright FullName
                                FamilyName Weight PostScript
                                BaseFontName FontName))])
    (hash-update! top-dict key
                  (λ (tdk-val) (add-string this (CFFont-string (cff-subset-cff this) tdk-val)))))
  (hash-set! top-dict 'ROS (list (add-string this "Adobe") (add-string this "Identity") 0))
  (hash-set! top-dict 'CIDCount (length (cff-subset-charstrings this)))

  (if (hash-ref (cff-subset-cff this) 'isCIDFont)
      (subset-font-dict this top-dict)
      (create-cid-fontdict this top-dict))

  (define top
    (mhasheq 'version 1
             'hdrSize (hash-ref (cff-subset-cff this) 'hdrSize)
             'offSize 4
             'header (hash-ref (cff-subset-cff this) 'header #f)
             'nameIndex (list (CFFFont-postscriptName (cff-subset-cff this)))
             'topDictIndex (list top-dict)
             'stringIndex (cff-subset-strings this)
             'globalSubrIndex (cff-subset-gsubrs this)))

  (for ([k (in-list (sort (dict-keys top-dict) symbol<?))])
    (match (dict-ref top-dict k)
      [(or (? list? (? dict?))) k]
      [val val]))
  
  (encode CFFTop top stream))

#;(module+ test
    (require "font.rkt" "helper.rkt")
    (define otf (open-font (path->string fira-otf-path)))
    (define cffss (+cff-subset otf))
    cffss)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/subset/TTFSubset.js
|#

(struct ttf-subset subset (glyf offset loca hmtx) #:transparent #:mutable)

(define (+ttf-subset font [glyphs empty] [mapping (mhash)]
                     [glyf #f]
                     [offset #f]
                     [loca #f]
                     [hmtx #f])
  (define ss (ttf-subset font glyphs mapping glyf offset loca hmtx))
  (subset-add-glyph! ss 0)
  ss)


(define (ttf-subset-add-glyph ss gid)
  ;; glyph-decode unpacks the `glyf` table data corresponding to a certin gid.
  ;; here, it's not necessary for non-composite glyphs
  ;; because they just get copied entirely into the subset.
  ;; it's just used to detect composite glyphs and handle them specially.
  ;; so an optimization would be to detect composite / noncomposite without full glyph-decode.
  (define glyph (get-glyph (subset-font ss) gid))
  (define ttf-glyf-data (glyph-decode glyph))
  
  ;; get the offset to the glyph from the loca table
  (match-define (list this-offset next-offset)
    (take (drop (hash-ref (get-table (subset-font ss) 'loca) 'offsets) gid) 2))

  (define port (get-table-stream (subset-font ss) 'glyf))
  (pos port (+ (pos port) this-offset))
  (define glyf-bytes (read-bytes (- next-offset this-offset) port))

  ;; if it is a compound glyph, include its components
  (when (and ttf-glyf-data (negative? (hash-ref ttf-glyf-data 'numberOfContours)))
    (for ([ttf-glyph-component (in-list (hash-ref ttf-glyf-data 'components))])
      (define gid (subset-add-glyph! ss (ttf-glyph-component-glyph-id ttf-glyph-component)))
      ;; note: this (ttf-glyph-component-pos component) is correct. It's a field of a Component object, not a port
      (bytes-copy! glyf-bytes (ttf-glyph-component-pos ttf-glyph-component) (encode uint16be gid #f))))

  ;; `loca` table v0 stores offsets as half of actual value
  ;; so we need an even number of bytes to encode
  (define glyf-bytes-even (if (odd? (bytes-length glyf-bytes))
                              (bytes-append glyf-bytes #"0")
                              glyf-bytes))
  
  (set-ttf-subset-glyf! ss (append (ttf-subset-glyf ss) (list glyf-bytes-even)))
  (hash-update! (ttf-subset-loca ss) 'offsets
                (λ (os)
                  (append os (list (ttf-subset-offset ss)))))
  (hash-update! (ttf-subset-hmtx ss) 'metrics
                (λ (ms) (append ms
                                (list (mhash 'advance (glyph-advance-width glyph)
                                             'bearing (hash-ref (get-glyph-metrics glyph) 'leftBearing))))))
  (set-ttf-subset-offset! ss (+ (ttf-subset-offset ss) (bytes-length glyf-bytes-even)))
  (sub1 (length (ttf-subset-glyf ss))))

;; tables required by PDF spec:
;; head, hhea, loca, maxp, cvt, prep, glyf, hmtx, fpgm
;; additional tables required for standalone fonts:
;; name, cmap, OS/2, post

(define (clone-deep val) (deserialize (serialize val)))

(define (ttf-subset-encode ss port)  
  (set-ttf-subset-glyf! ss empty)
  (set-ttf-subset-offset! ss 0)
  (set-ttf-subset-loca! ss (mhash 'offsets empty))
  (set-ttf-subset-hmtx! ss (mhash 'metrics empty 'bearings empty))

  ;; include all the glyphs used in the document
  ;; not using `in-list` because we need to support adding more
  ;; glyphs to the array as component glyphs are discovered & enqueued
  (for ([idx (in-naturals)]
        #:break (= idx (length (subset-glyphs ss))))
    (define gid (list-ref (subset-glyphs ss) idx))
    (ttf-subset-add-glyph ss gid))
  
  (define new-maxp-table (clone-deep (get-maxp-table (subset-font ss))))
  (dict-set! new-maxp-table 'numGlyphs (length (ttf-subset-glyf ss)))
  
  ;; populate the new loca table
  (dict-update! (ttf-subset-loca ss) 'offsets (λ (vals) (append vals (list (ttf-subset-offset ss)))))
  (loca-pre-encode (ttf-subset-loca ss))

  (define new-head-table (clone-deep (get-head-table (subset-font ss))))
  (dict-set! new-head-table 'indexToLocFormat (dict-ref (ttf-subset-loca ss) x:version-key))
  
  (define new-hhea-table (clone-deep  (get-hhea-table (subset-font ss))))
  (dict-set! new-hhea-table 'numberOfMetrics (length (dict-ref (ttf-subset-hmtx ss) 'metrics)))

  (define new-tables
    (filter cdr (dictify 'head new-head-table
                         'hhea new-hhea-table
                         'loca (ttf-subset-loca ss)
                         'maxp new-maxp-table
                         'cvt_ (and (has-table? (subset-font ss) 'cvt_)
                                    (get-cvt_-table (subset-font ss)))
                         'prep (and (has-table? (subset-font ss) 'prep)
                                    (get-prep-table (subset-font ss)))
                         'glyf (ttf-subset-glyf ss)
                         'hmtx (ttf-subset-hmtx ss)
                         'fpgm (and (has-table? (subset-font ss) 'fpgm)
                                    (get-fpgm-table (subset-font ss))))))
  
  (encode Directory (mhash 'tables new-tables) port)
  (void))

