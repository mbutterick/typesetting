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
   encode)

  
  )

(define-stub-go _addGlyph)

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

  ;; todo: final encoding of directory, with all tables.
  (send Directory encode stream
        (mhash 'tables
               (mhash
                'head head
                'hhea hhea
                'loca (· this loca)
                'maxp maxp
                ;; todo: cvt
                'prep (send (· this font) _getTable 'prep)
                ;; 'glyf (· this glyf)
                ;; 'hmtx (· this hmtx)
                'fpgm (send (· this font) _getTable 'fpgm)
                )))

  (report* stream (send stream dump))
  
  (unfinished)
  )


