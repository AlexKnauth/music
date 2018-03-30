#lang agile

;; Based on the segment labeling algorithm from the paper:

;; Algorithms for Chordal Analysis
;; by Bryan Pardo and William P. Birmingham
;; published in the Computer Music Journal on July 23, 2002

(require racket/dict
         music/util/filter-maximal
         (prefix-in nc/ "../note/note-class.rkt")
         "../note/main.rkt"
         "../time/main.rkt"
         "../chord/chord-symbol.rkt"
         "../score/main.rkt"
         "../scale/scale.rkt"
         "partition-point.rkt"
         "infer-segment-chord.rkt"
         "infer-segmentation.rkt"
         (submod "../note/note.rkt" example)
         (submod "../chord/chord-symbol.rkt" example))
(module+ test
  (require rackunit
           (submod "../note/note-held.rkt" example)))

;; ------------------------------------------------------------------------

(provide analyze-chords)

;; A BeatStrengths is a [Listof [Pair Duration Real]]

;; analyze-chords : Score -> [Listof [Timed [Maybe ChordSymbol]]]
(define (analyze-chords s)
  (let loop ([ms (score-notes-split-measures s)]
             [i 0]
             [k (key 0)]
             [ts #f]
             [acc '()])
    (match ms
      ['() (reverse acc)]
      [(cons m ms)
       (define key
         (or
          (for/last ([e (in-list m)]
                     #:when (key-there? e))
            (timed-value e))
          k))
       (define time-sig
         (or
          (for/last ([e (in-list m)]
                     #:when (time-sig-there? e))
            (timed-value e))
          ts))
       (loop
        ms
        (add1 i)
        key
        time-sig
        (append (analyze-chords/measure m key)
                acc))])))

;; ------------------------------------------------------------------------

;; analyze-chords/measure :
;; [Listof MusElementThere] Nat -> [Listof [Timed [Maybe ChordSymbol]]]
(define (analyze-chords/measure notes key)
  (infer-segmentation (filter note-there? notes) key))

;; ------------------------------------------------------------------------

