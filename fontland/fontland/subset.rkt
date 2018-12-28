#lang debug racket/base
(require racket/serialize
         racket/class
         racket/list
         racket/match
         sugar/unstable/dict
         sugar/unstable/js
         "table/loca.rkt"
         "table-stream.rkt"
         "directory.rkt"
         "struct.rkt"
         fontland/glyph
         fontland/ttf-glyph
         xenomorph
         racket/dict)

(provide subset +subset ttf-subset +ttf-subset subset-add-glyph! encode-to-port create-subset)

#|
approximates
https://github.com/devongovett/fontkit/blob/master/src/subset/Subset.js
|#

; glyphs = list of glyph ids in the subset
; mapping = of glyph ids to indexes in glyphs
(struct subset (font glyphs mapping) #:transparent #:mutable)

(define (+subset font [glyphs empty] [mapping (mhash)])
  (define ss (subset font glyphs mapping))
  (subset-add-glyph! ss 0)
  ss)

(define (encode-to-port ss)
  (define p (open-output-bytes))
  (subset-encode ss p)
  p)

(define (subset-add-glyph! ss glyph-or-gid)
  (define new-gid ((if (glyph? glyph-or-gid) glyph-id values) glyph-or-gid))
  ;; put the new glyph at the end of `glyphs`,
  ;; and put its index in the mapping
  (hash-ref! (subset-mapping ss) new-gid
             (λ ()
               (set-subset-glyphs! ss (append (subset-glyphs ss) (list new-gid)))
               (sub1 (length (subset-glyphs ss))))))  


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

(define (create-subset font)
  ((if (has-table? font #"CFF_")
       (error 'cff-fonts-unsupported)
       +ttf-subset) font))

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
  
  (set-ttf-subset-glyf! ss (append (ttf-subset-glyf ss) (list glyf-bytes)))
  (hash-update! (ttf-subset-loca ss) 'offsets
                (λ (os)
                  (append os (list (ttf-subset-offset ss)))))
  (hash-update! (ttf-subset-hmtx ss) 'metrics
                (λ (ms) (append ms
                                (list (mhash 'advance (glyph-advance-width glyph)
                                             'bearing (hash-ref (get-glyph-metrics glyph) 'leftBearing))))))
  (set-ttf-subset-offset! ss (+ (ttf-subset-offset ss) (bytes-length glyf-bytes)))
  (sub1 (length (ttf-subset-glyf ss))))

;; tables required by PDF spec:
;; head, hhea, loca, maxp, cvt, prep, glyf, hmtx, fpgm
;; additional tables required for standalone fonts:
;; name, cmap, OS/2, post

(define (clone-deep val) (deserialize (serialize val)))

(define (subset-encode ss port)  
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
    (let ()
      (define kvs (dictify 'head new-head-table
                           'hhea new-hhea-table
                           'loca (ttf-subset-loca ss)
                           'maxp new-maxp-table
                           'cvt_ (get-cvt_-table (subset-font ss))
                           'prep (get-prep-table (subset-font ss))
                           'glyf (ttf-subset-glyf ss)
                           'hmtx (ttf-subset-hmtx ss)
                           'fpgm (get-fpgm-table (subset-font ss))))
      (for ([(k v) (in-dict kvs)]
            #:unless v)
        (error 'encode (format "missing value for ~a" k)))
      (make-hasheq kvs)))
  
  (encode Directory (mhash 'tables new-tables) port)
  (void))

