#lang debug racket/base
(require racket/serialize
         racket/contract
         racket/class
         racket/list
         racket/match
         sugar/unstable/class
         sugar/unstable/dict
         sugar/unstable/js
         "table/loca.rkt"
         "directory.rkt"
         "helper.rkt"
         fontland/glyph
         fontland/ttf-glyph
         xenomorph)

(provide Subset +Subset TTFSubset +TTFSubset includeGlyph encode-to-port)

#|
approximates
https://github.com/devongovett/fontkit/blob/master/src/subset/Subset.js
|#

#;(define-subclass object% (Subset font)
    (field [glyphs empty] ; list of glyph ids in the subset
           [mapping (mhash)]) ; mapping of glyph ids to indexes in `glyphs`
    (send this includeGlyph 0) ; always include the missing glyph in subset
    (as-methods
     includeGlyph))

; glyphs = list of glyph ids in the subset
; mapping = of glyph ids to indexes in `glyphs`
(struct Subset (font glyphs mapping) #:transparent #:mutable)

(define (+Subset font [glyphs empty] [mapping (mhash)])
  (define ss (Subset font glyphs mapping))
  (includeGlyph ss 0)
  ss)

(define (encode-to-port ss)
  (define p (open-output-bytes))
  (encode ss p)
  p)

(define (includeGlyph ss glyph-or-gid)
  #;((or/c object? index?) . ->m . index?)
  (define glyph (if (object? glyph-or-gid)
                    (· glyph-or-gid id)
                    glyph-or-gid))
  (hash-ref! (Subset-mapping ss) glyph
             (λ ()
               ;; put the new glyph at the end of `glyphs`,
               ;; and put its index in the mapping
               (set-Subset-glyphs! ss (append (Subset-glyphs ss) (list glyph)))
               (sub1 (length (Subset-glyphs ss))))))  

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/subset/CFFSubset.js
|#


#|
;; no CFF font support for now

(define-subclass Subset (CFFSubset)
  #R (· this font)
  (field [cff (send (· this font) _getTable 'CFF_)])
  (unless (· this cff) (error 'not-a-cff-font))
  (field [charStrings #f]
         [subrs #f])

  (as-methods
   subsetCharstrings
   #;subsetSubrs
   #;subsetFontdict
   #;createCIDFontdict
   #;addString
   #;encode))

(define/contract (subsetCharstrings this)
  (->m void?)
  (set-field! charStrings this null)
  (define gsubrs (make-hash))
  (for ([gid (in-list (· this glyphs))])
    (push-end-field! charStrings this (· this cff getCharString gid))
    (define glyph (· this font getGlyph gid))
    (define path (· glyph path)) ; this causes the glyph to be parsed
    (for ([subr (in-list (· glyph _usedGsubrs))])
      (hash-set! gsubrs subr #true)))
  (set-field! this gsubrs (send this subsetSubrs (· this cff globalSubrIndex) gsubrs))
  (void))


|#

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/subset/TTFSubset.js
|#

#;(define-subclass Subset (TTFSubset)
    (field [glyf #f]
           [offset #f]
           [loca #f]
           [hmtx #f])

    (as-methods
     _addGlyph
     encode))

(struct TTFSubset Subset (glyf offset loca hmtx) #:transparent #:mutable)

(define (+TTFSubset font [glyphs empty] [mapping (mhash)]
                    [glyf #f]
                    [offset #f]
                    [loca #f]
                    [hmtx #f])
  (define ss (TTFSubset font glyphs mapping glyf offset loca hmtx))
  (includeGlyph ss 0)
  ss)

(define (_addGlyph ss gid)
  #;(index? . ->m . index?)

  (define glyph (send (Subset-font ss) getGlyph gid))
  ;; glyph-decode unpacks the `glyf` table data corresponding to a certin gid.
  ;; here, it's not necessary for non-composite glyphs
  ;; because they just get copied entirely into the subset.
  ;; it's just used to detect composite glyphs and handle them specially.
  ;; so an optimization would be to detect composite / noncomposite without full glyph-decode.
  (define ttf-glyf-data (glyph-decode glyph))

  ;; get the offset to the glyph from the loca table
  (match-define (list curOffset nextOffset) (take (drop (· (Subset-font ss) loca offsets) gid) 2))

  (define port (send (Subset-font ss) _getTableStream 'glyf))
  (pos port (+ (pos port) curOffset))

  (define buffer (read-bytes  (- nextOffset curOffset) port))

  ;; if it is a compound glyph, include its components
  (when (and ttf-glyf-data (negative? (· ttf-glyf-data numberOfContours)))
    (for ([ttf-glyph-component (in-list (· ttf-glyf-data components))])
      (define gid (includeGlyph ss (ttf-glyph-component-glyph-id ttf-glyph-component)))
      ;; note: this (ttf-glyph-component-pos component) is correct. It's a field of a Component object, not a port
      (bytes-copy! buffer (ttf-glyph-component-pos ttf-glyph-component) (send uint16be encode #f gid))))
  
  ;; skip variation shit

  (set-TTFSubset-glyf! ss (append (TTFSubset-glyf ss) (list buffer)))
  (hash-update! (TTFSubset-loca ss) 'offsets (λ (os)
                                                 (append os (list (TTFSubset-offset ss)))))

  (hash-update! (TTFSubset-hmtx ss) 'metrics (λ (ms) (append ms
                                                               (list (mhash 'advance (glyph-advance-width glyph)
                                                                            'bearing (· (get-glyph-metrics glyph) leftBearing))))))
  
  (set-TTFSubset-offset! ss (+ (TTFSubset-offset ss) (bytes-length buffer)))
  (sub1 (length (TTFSubset-glyf ss))))

;; tables required by PDF spec:
;; head, hhea, loca, maxp, cvt, prep, glyf, hmtx, fpgm
;; additional tables required for standalone fonts:
;; name, cmap, OS/2, post

(define (cloneDeep val)  (deserialize (serialize val)))

(define (encode ss port)
  #;(output-port? . ->m . void?)
  
  (set-TTFSubset-glyf! ss empty)
  (set-TTFSubset-offset! ss 0)
  (set-TTFSubset-loca! ss (mhash 'offsets empty))
  (set-TTFSubset-hmtx! ss (mhash 'metrics empty 'bearings empty))

  ;; include all the glyphs used in the document
  ;; not using `in-list` because we need to support adding more
  ;; glyphs to the array as component glyphs are discovered & enqueued
  (for ([idx (in-naturals)]
        #:break (= idx (length (Subset-glyphs ss))))
    (define gid (list-ref (Subset-glyphs ss) idx))
    (_addGlyph ss gid))

  (define maxp (cloneDeep (· (Subset-font ss) maxp to-hash)))
  (dict-set! maxp 'numGlyphs (length (TTFSubset-glyf ss)))
  
  ;; populate the new loca table
  (dict-update! (TTFSubset-loca ss) 'offsets (λ (vals) (append vals (list (TTFSubset-offset ss)))))
  (loca-pre-encode (TTFSubset-loca ss))

  (define head (cloneDeep (· (Subset-font ss) head to-hash)))
  (dict-set! head 'indexToLocFormat (· (TTFSubset-loca ss) version))
  
  (define hhea (cloneDeep (· (Subset-font ss) hhea to-hash)))
  (dict-set! hhea 'numberOfMetrics (length (· (TTFSubset-hmtx ss) metrics)))

  
  (send Directory encode port
        (mhash 'tables
               (mhash
                'head head
                'hhea hhea
                'loca (TTFSubset-loca ss)
                'maxp maxp
                'cvt_ (· (Subset-font ss) cvt_)
                'prep (· (Subset-font ss) prep)
                'glyf (TTFSubset-glyf ss)
                'hmtx (TTFSubset-hmtx ss)
                'fpgm (· (Subset-font ss) fpgm))))

  #;(report* (bytes-length (send stream dump)) (send stream dump))
  #;(report* (bytes-length (file->bytes "out.bin")) (file->bytes "out.bin"))
  
  (void)
  )

