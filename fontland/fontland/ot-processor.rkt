#lang racket/base
(require "racket.rkt")

(require (prefix-in Script- "script.rkt") br/cond "glyph-iterator.rkt")
(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/opentype/OTProcessor.js
|#


(define DEFAULT_SCRIPTS '(DFLT dflt latn))

(define-subclass object% (OTProcessor font table)
  (field [script #f]
         [scriptTag #f]
         [language #f]
         [languageTag #f]
         [features (mhash)]
         [lookups (mhash)]
         [direction #f]
         [glyphIterator #f]) ; appears below

  ;; initialize to default script + language
  (selectScript)

  ;; current context (set by applyFeatures)
  (field [glyphs empty]
         [positions empty] ; only used by GPOS
         [ligatureID 1])

  (define/public (findScript script-or-scripts)
    (and (· this table scriptList)
         (let ([scripts (if (pair? script-or-scripts) script-or-scripts (list script-or-scripts))])
           (for*/first ([entry (in-list (· this table scriptList))]
                        [s (in-list scripts)]
                        #:when (eq? (· entry tag) s))
             entry))))


  (define/public (selectScript [script #f] [language #f])
    (let/ec return!
      (define changed #f)
      (define entry #f)
      (when (or (not (· this script)) (not (eq? script (· this scriptTag))))
        (set! entry (findScript script))
        (when script
          (set! entry (findScript script))) ; ? why double dip
        (when (not entry)
          (set! entry (findScript DEFAULT_SCRIPTS)))
        (when (not entry)
          (return! (void)))
        (set-field! scriptTag this (· entry tag))
        (set-field! script this (· entry script))
        (set-field! direction this (Script-direction script))
        (set-field! language this #f)
        (set! changed #t))

      (when (and (not language) (not (equal? language (· this languageTag))))
        (for/first ([lang (in-list (· this script langSysRecords))]
                    #:when (equal? (· lang tag) language))
          (set-field! language this (· lang langSys))
          (set-field! languageTag this (· lang tag))
          (set! changed #t)))

      (when (not (· this language))
        (set-field! language this (· this script defaultLangSys)))

      ;; Build a feature lookup table
      (when changed
        (set-field! features this (mhash))
        (when (· this language)
          (for ([featureIndex (in-list (· this language featureIndexes))])
            (define record (list-ref (· this table featureList) featureIndex))
            (dict-set! (· this features) (· record tag) (· record feature)))))))

  
  (define/public (lookupsForFeatures [userFeatures empty] [exclude #f])
    #;(report*/file 'ot-proc:lookupsForFeatures)
    (sort (for*/list ([tag (in-list userFeatures)]
                      [feature (in-value (dict-ref (· this features) tag #f))]
                      #:when feature
                      [lookupIndex (in-list (· feature lookupListIndexes))]
                      #:unless (and exclude (index-of exclude lookupIndex)))
            #;(report*/file tag lookupIndex)
            (mhasheq 'feature tag
                     'index lookupIndex
                     'lookup (send (· this table lookupList) get lookupIndex)))
          < #:key (λ (i) (· i index))))
      

  (define/public (applyFeatures userFeatures glyphs advances)
    #;(report/file 'ot-proc:applyFeatures-part1)
    (define lookups (send this lookupsForFeatures userFeatures))
    #;(report/file 'ot-proc:applyFeatures-part2)
    #;(report/file (length glyphs))
    (send this applyLookups lookups glyphs advances)
    #;(report*/file (length glyphs) (length (· this glyphs)))
    (· this glyphs))

  
  (define/public (applyLookups lookups glyphs positions)
    (set-field! glyphs this glyphs)
    (set-field! positions this positions)
    #;(report/file 'ot-proc:applyLookups)
    #;(report (for/list ([g (· this glyphs)]) (· g id)) 'glyph-ids)
    (set-field! glyphIterator this (+GlyphIterator glyphs))
    
    (for* ([lookup-entry (in-list lookups)])
      (define feature (· lookup-entry feature))
      (define lookup (· lookup-entry lookup))
      #;(report 'resetting-iterator)
      (send (· this glyphIterator) reset (· lookup flags))
      
      (while (< (or (· this glyphIterator index) 0) (length (· this glyphs)))
             #;(report/file 'start-while++++++++++++++++++)
             #;(report (length (· this glyphs)) 'glyphs-length-top)
             #;(report (for/list ([g (· this glyphs)]) (· g id)) 'gids-top)
             #;(report (· this glyphIterator index) giterator-idx-top)
             #;(report* feature (dict-keys (· this glyphIterator cur features)))
             #;(report (dict-has-key? (· this glyphIterator cur features) feature))
             (cond
               [(not (dict-has-key? (· this glyphIterator cur features) feature))
                (send (· this glyphIterator) next)]
               [else
                #;(report 'start-lookup-branch=================)
                #;(report* (for/list ([g glyphs]) (· g id)) (for/list ([g (· this glyphs)]) (· g id)) (for/list ([g (· this glyphIterator glyphs)]) (· g id))  (· this glyphIterator index) (· this glyphIterator cur id) (· this glyphIterator peekIndex))
                (for/or ([table (in-list (· lookup subTables))])
                  (send this applyLookup (· lookup lookupType) table))
                #;(report 'incrementing-iterator-at-bottom)
                (send (· this glyphIterator) next)
                #;(report* (· this glyphIterator cur) (· this glyphIterator index))
                (· this glyphIterator index)]))))

  (abstract applyLookup)

  (define/public (applyLookupList lookupRecords)
    (report/file 'applyLookupList-not-implemented)
    (error))

  (define/public (coverageIndex coverage [glyph #f])
    (unless glyph (set! glyph (· this glyphIterator cur id)))
    (or (case (· coverage version)
          [(1) (index-of (· coverage glyphs) glyph)]
          [(2) (for/first ([range (in-list (· coverage rangeRecords))]
                           #:when (<= (· range start) glyph (· range end)))
                 (+ (· range startCoverageIndex) glyph (- (· range start))))]
          [else #f]) -1))

  (define/public (match sequenceIndex sequence fn [matched #f])
    (define pos (· this glyphIterator index))
    (define glyph (send (· this glyphIterator) increment sequenceIndex))
    (define idx 0)
    #;(report*/file (and (pair? sequence) (list-ref sequence idx)) glyph (and glyph (· glyph id)))

    (while (and (< idx (length sequence)) glyph (fn (list-ref sequence idx) (· glyph id)))
           #;(report* 'in-match-loop idx (· glyph id))
           (when matched
             (push-end! matched (· this glyphIterator index)))
           (increment! idx)
           (set! glyph (· this glyphIterator next)))

    (set-field! index (· this glyphIterator) pos)
    (cond
      [(< idx (length sequence)) #f]
      [else (or matched #t)]))
  
  (define/public (sequenceMatchIndices sequenceIndex sequence)
    (send this match sequenceIndex sequence (λ (component glyph) (= component glyph)) empty))

  (define/public (coverageSequenceMatches sequenceIndex sequence)
    #;(report 'in-coverageSequenceMatches)
    (send this match sequenceIndex sequence (λ (coverage glyph) (>= (send this coverageIndex coverage glyph) 0))))

  (define/public (getClassID glyph classDef)
    (or
     (case (· classDef version)
       [(1) ;; Class array
        (define i (- glyph (· classDef startGlyph)))
        (and (>= i 0)
             (< i (length (· classDef classValueArray)))
             (list-ref (· classDef classValueArray) i))]
       [(2)
        (for/first ([range (in-list (· classDef classRangeRecord))]
                    #:when (<= (· range start) glyph (· range end)))
          (· range class))])
     0))

  (define/public (applyContext table)
    (report/file 'otproc:applyContext)
    (case (· table version)
      [(1) (define index (send this coverageIndex (· table coverage)))
           (cond
             [(= index -1) #f]
             [else (define set (list-ref (· table ruleSets) index))
                   (for/first ([rule (in-list set)]
                               #:when (send this sequenceMatches 1 (· rule input)))
                     (send this applyLookupList (· rule lookupRecords)))])]
      [(2) (cond
             [(= (send this coverageIndex (· table coverage)) -1) #f]
             [else (define index
                     (send this getClassID (· this glyphIterator cur id) (· table classDef)))
                   (cond
                     [(- index -1) #f]
                     [else (define set (list-ref (· table classSet) index))
                           (for/first ([rule (in-list set)]
                                       #:when (send this sequenceMatches 1 (· rule classes) (· table classDef)))
                             (send this applyLookupList (· rule lookupRecords)))])])]

      [(3) (and (send this coverageSequenceMatches 0 (· table coverages))
                (send this applyLookupList (· table lookupRecords)))]
      [else #f]))

(define/public (applyChainingContext table)
  #;(report/file 'otproc:applyChainingContext)
  (case (· table version)
    [(1)
     #;(report 'case-1)
     (define index (send this coverageIndex (· table coverage)))
         (cond
           [(= index -1) #f]
           [else (define set (list-ref (· table chainRuleSets) index))
                 (for/first ([rule (in-list set)]
                       #:when (and (send this sequenceMatches (- (length (· rule backtrack)) (· rule backtrack)))
                                   (send this sequenceMatches 1 (· rule input))
                                   (send this sequenceMatches (add1 (length (· rule input))) (· rule lookahead))))
                   (send this applyLookupList (· rule lookupRecords)))])]
    [(2)
     #;(report 'case-2)
     (cond
           [(= -1 (send this coverageIndex (· table coverage))) #f]
           [else (define index (send this getClassID (· this glyphIterator cur id) (· table inputClassDef)))
                 (define rules (list-ref (· table chainClassSet) index))
                 (cond
                   [(not rules) #f]
                   [else (for/first ([rule (in-list rules)]
                               #:when (and (send this classSequenceMatches (- (length (· rule backtrack)) (· rule backtrack) (· table backtrackClassDef)))
                                   (send this classSequenceMatches 1 (· rule input) (· table inputClassDef))
                                   (send this classSequenceMatches (add1 (length (· rule input))) (· rule lookahead) (· table lookaheadClassDef))))
                           (send this applyLookupList (· rule lookupRecords)))])])]
    [(3)
     #;(report 'case-3)
     (and
          (send this coverageSequenceMatches (- (· table backtrackGlyphCount)) (· table backtrackCoverage))
          (send this coverageSequenceMatches 0 (· table inputCoverage))
          (send this coverageSequenceMatches (· table inputGlyphCount) (· table lookaheadCoverage))
          (send this applyLookupList (· table lookupRecords)))]
    [else #f]))

  )
    
    