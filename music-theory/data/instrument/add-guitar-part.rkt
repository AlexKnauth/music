#lang agile

(require "../score/score.rkt"
         "../note-held.rkt"
         "../position.rkt"
         "../chord/chord-symbol.rkt"
         "../chord/infer-chord.rkt"
         "../instrument/string-spec.rkt"
         "../instrument/chord-fingering.rkt")

;; ------------------------------------------------------------------------

(provide score-add-guitar-part)

;; score-add-guitar-part : Score -> Score
(define (score-add-guitar-part s)
  (define measure-length
    (score-measure-length s))
  ;; one per measure
  (define harmony-elements
    (for/list ([chord-symbol (in-list (analyze-chords s))])
      (harmony-element
       chord-symbol
       (first (min-stretch-chord-layouts
               guitar-strings
               (chord-symbol->chord chord-symbol))))))
  (score-add-part
   s
   (part "Guitar"
     (sorted/position
      (for/list ([harmony-element (in-list harmony-elements)]
                 [measure-num (in-naturals)])
        (define chord
          (chord-layout->chord guitar-strings
                               (harmony-element-chord-layout harmony-element)))
        (apply here (position measure-num beat-one)
          harmony-element
          (for/list ([n (in-list chord)])
            (note-held n measure-length))))))))

;; ------------------------------------------------------------------------

