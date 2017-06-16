#lang fontkit/racket
(require "clone.rkt" "ttfglyphencoder.rkt" "loca.rkt" "directory.rkt" restructure)
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

  (define/public (encodeStream)
    (define s (+EncodeStream))
    (send this encode s)
    s)
  
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
  (define glyf (send glyph _decode))

  ;; get the offset to the glyph from the loca table
  (define loca (send (· this font) _getTable 'loca))
  (define curOffset (list-ref (· loca offsets) gid))
  (define nextOffset (list-ref (· loca offsets) (add1 gid)))

  (define stream (send (· this font) _getTableStream 'glyf))
  (send stream pos (+ (send stream pos) curOffset))

  (define buffer (send stream readBuffer (- nextOffset curOffset)))

  ;; if it is a compound glyph, include its components
  (when (and glyf (negative? (· glyf numberOfContours)))
    (set! buffer (+Buffer buffer))
    (for ([component (in-list (· glyf components))])
      (define gid (includeGlyph (· component glyphID)))
      (send buffer writUInt16BE gid (send component pos))))
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
  (EncodeStream? . ->m . void?)
  (set-field! glyf this empty)
  (set-field! offset this 0)
  (set-field! loca this (mhash 'offsets empty))
  (set-field! hmtx this (mhash 'metrics empty 'bearings empty))

  ;; include all the glyphs used in the document
  (for ([gid (in-list (· this glyphs))])
    (send this _addGlyph gid))

  (define maxp (cloneDeep (send (· this font) _getTable 'maxp)))
  (hash-set! maxp 'numGlyphs (length (· this glyf)))

  ;; populate the new loca table
  (hash-update! (· this loca) 'offsets (λ (vals) (append vals (list (· this offset)))))
  (loca-preEncode (· this loca))

  (define head (cloneDeep (send (· this font) _getTable 'head)))
  (hash-set! head 'indexToLocFormat (· this loca version))
  
  (define hhea (cloneDeep (send (· this font) _getTable 'hhea)))
  (hash-set! hhea 'numberOfMetrics (length (· this hmtx metrics)))

  (send Directory encode stream
        (mhash 'tables
               (mhash
                'head head
                'hhea hhea
                'loca (· this loca)
                'maxp maxp
                'cvt_ (send (· this font) _getTable 'cvt_)
                'prep (send (· this font) _getTable 'prep)
                'glyf (· this glyf)
                'hmtx (· this hmtx)
                'fpgm (send (· this font) _getTable 'fpgm)
                )))

  #;(report* (bytes-length (send stream dump)))
  
  (void)
  )


