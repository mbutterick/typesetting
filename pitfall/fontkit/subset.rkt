#lang fontkit/racket
(require "clone.rkt" "ttfglyphencoder.rkt" "loca.rkt" "directory.rkt" xenomorph)
(provide Subset CFFSubset TTFSubset)

#|
approximates
https://github.com/devongovett/fontkit/blob/master/src/subset/Subset.js
|#

(define-subclass object% (Subset font)
  (field [glyphs empty] ; list of glyph ids in the subset
         [mapping (mhash)] ; mapping of glyph ids to indexes in `glyphs`
         )

  (send this includeGlyph 0) ; always include the missing glyph in subset


  
  (as-methods
   includeGlyph))

(define/contract (includeGlyph this glyph)
  ((or/c object? index?) . ->m . index?)
  (let ([glyph (if (object? glyph) (· glyph id) glyph)])
    (hash-ref! (· this mapping) glyph
               (λ ()
                 ;; put the new glyph at the end of `glyphs`,
                 ;; and put its index in the mapping
                 (push-end-field! glyphs this glyph)
                 (sub1 (length (· this glyphs)))))))  


(define-subclass Subset (CFFSubset)
  (error 'cff-subset-unimplemented))


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/subset/TTFSubset.js
|#

(define-subclass Subset (TTFSubset)
  (field [glyphEncoder (make-object TTFGlyphEncoder)])
  (field [glyf #f]
         [offset #f]
         [loca #f]
         [hmtx #f])

  (as-methods
   _addGlyph
   encode))

(define/contract (_addGlyph this gid)
  (index? . ->m . index?)

  (define glyph (send (· this font) getGlyph gid))
  ;; _decode unpacks the `glyf` table data corresponding to a certin gid.
  ;; here, it's not necessary for non-composite glyphs
  ;; because they just get copied entirely into the subset.
  ;; it's just used to detect composite glyphs and handle them specially.
  ;; so an optimization would be to detect composite / noncomposite without full _decode.
  (define glyf (send glyph _decode))

  ;; get the offset to the glyph from the loca table
  (match-define (list curOffset nextOffset) (take (drop (· this font loca offsets) gid) 2))

  (define stream (send (· this font) _getTableStream 'glyf))
  (send stream pos (+ (send stream pos) curOffset))

  (define buffer (send stream readBuffer (- nextOffset curOffset)))

  ;; if it is a compound glyph, include its components
  (when (and glyf (negative? (· glyf numberOfContours)))
    (for ([component (in-list (· glyf components))])
      (define gid (send this includeGlyph (· component glyphID)))
      (bytes-copy! buffer (· component pos) (send uint16be encode #f gid))))
  
  ;; skip variation shit

  (push-end-field! glyf this buffer)
  (hash-update! (get-field loca this) 'offsets (λ (os)
                                                 (append os (list (get-field offset this)))))

  (hash-update! (get-field hmtx this) 'metrics (λ (ms) (append ms
                                                               (list (mhash 'advance (· glyph advanceWidth)
                                                                            'bearing (· (send glyph _getMetrics) leftBearing))))))

  (increment-field! offset this (bytes-length buffer))
  (sub1 (length (· this glyf))))

;; tables required by PDF spec:
;; head, hhea, loca, maxp, cvt, prep, glyf, hmtx, fpgm
;; additional tables required for standalone fonts:
;; name, cmap, OS/2, post

(define/contract (encode this stream)
  (output-port? . ->m . void?)
  (set-field! glyf this empty)
  (set-field! offset this 0)
  (set-field! loca this (mhash 'offsets empty))
  (set-field! hmtx this (mhash 'metrics empty 'bearings empty))

  ;; include all the glyphs used in the document
  ;; not using `in-list` because we need to support adding more
  ;; glyphs to the array as component glyphs are discovered & enqueued
  (for ([idx (in-naturals)]
        #:break (= idx (length (· this glyphs))))
    (define gid (list-ref (· this glyphs) idx))
    (send this _addGlyph gid))

  (define maxp (cloneDeep (send (· this font maxp) dump)))
  (dict-set! maxp 'numGlyphs (length (· this glyf)))
  ;; populate the new loca table
  (dict-update! (· this loca) 'offsets (λ (vals) (append vals (list (· this offset)))))
  (loca-pre-encode (· this loca))

  
  (define head (cloneDeep (send (· this font head) dump)))
  (dict-set! head 'indexToLocFormat (· this loca version))
  
  (define hhea (cloneDeep (send (· this font hhea) dump)))
  (dict-set! hhea 'numberOfMetrics (length (· this hmtx metrics)))

  (send Directory encode stream
        (mhash 'tables
               (mhash
                'head head
                'hhea hhea
                'loca (· this loca)
                'maxp maxp
                'cvt_ (· this font cvt_)
                'prep (· this font prep)
                'glyf (· this glyf)
                'hmtx (· this hmtx)
                'fpgm (· this font fpgm)
                )))

  #;(report* (bytes-length (send stream dump)) (send stream dump))
  #;(report* (bytes-length (file->bytes "out.bin")) (file->bytes "out.bin"))
  
  (void)
  )


