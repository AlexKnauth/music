#lang agile

(require "../score/score.rkt"
         "../score/key-signature.rkt"
         "../time/time-signature.rkt"
         "../note/note-held.rkt"
         "../time/position.rkt"
         "../time/duration.rkt"
         "../time/time-period.rkt"
         "../chord/chord-symbol.rkt"
         "../chord/infer-chord.rkt"
         "../instrument/string-spec.rkt"
         "../instrument/chord-fingering.rkt")

;; ------------------------------------------------------------------------

(provide score-add-guitar-part)

;; score-add-guitar-part : Score -> Score
(define (score-add-guitar-part s)
  ;; one per measure
  (define harmony-elements
    (for/list ([p (in-list (analyze-chords s))])
      (match-define (timed (time-period pos _) chord-symbol) p)
      (timed/pos
       pos
       (harmony-element
        chord-symbol
        (first (min-stretch-chord-layouts
                guitar-strings
                (chord-symbol->chord chord-symbol)))))))
  (score-add-part
   s
   (part "Guitar"
     (sorted/position
      (remove-duplicates (score-keys s))
      (remove-duplicates (score-time-sigs s))
      (for/list ([harmony-element (in-list harmony-elements)])
        (match-define (timed (time-period pos _) he) harmony-element)
        (define chord
          (chord-layout->chord guitar-strings
                               (harmony-element-chord-layout he)))
        (apply here pos
          he
          (for/list ([n (in-list chord)])
            ;; TODO: figure out durations
            (note-held n duration-quarter))))))))

;; ------------------------------------------------------------------------

;; score-keys : Score -> [Listof [WithPos Key]]
(define (score-keys s)
  (match s
    [(score _ parts)
     (define lsts (map part-keys parts))
     (define keys (first lsts))
     (unless (andmap (λ (x) (equal? x keys)) lsts)
       (error 'score-keys "different parts have different keys"))
     keys]))

;; score-time-sigs : Score -> [Listof [WithPos TimeSig]]
(define (score-time-sigs s)
  (match s
    [(score _ parts)
     (define lsts (map part-time-sigs parts))
     (define time-sigs (first lsts))
     (unless (andmap (λ (x) (equal? x time-sigs)) lsts)
       (error 'score-time-sigs "different parts have different time sigs"))
     time-sigs]))

(define (part-time-sigs p)
  (match p
    [(part _ elems)
     (filter time-sig-there? elems)]))

(define (part-keys p)
  (match p
    [(part _ elems)
     (filter key-there? elems)]))

;; ------------------------------------------------------------------------

