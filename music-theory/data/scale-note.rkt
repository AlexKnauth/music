#lang agile

(require "note.rkt")

;; ------------------------------------------------------------------------

(provide scale-note
         scale-note-number
         scale-note-degree
         scale-note->note
         note->scale-note)

;; A ScaleNote is a (scale-note Nat Int Int)
(struct scale-note [period number alteration] #:transparent)

;; scale-note-degree : ScaleNote -> Nat
(define (scale-note-degree sn)
  (match-define (scale-note N i alteration) sn)
  (modulo i N))

;; scale-note->note : Note ScaleKind ScaleNote -> Note
(define (scale-note->note root scale sn)
  (match sn
    [(scale-note N i alteration)
     (define degree (modulo i N))
     (define octave (/ (- i degree) N))
     (note-alteration+
      (note-octave+
       (note+ root (list-ref scale degree))
       octave)
      alteration)]))

;; note->scale-note : Note ScaleKind Note -> ScaleNote
(define (note->scale-note root scale n)
  (define N (length scale))
  (define n-ivl (note∆ root n))
  (define degree
    (for/first ([s-ivl (in-list scale)]
                [i (in-naturals)]
                #:when (ivl-name∆=? n-ivl s-ivl))
      i))
  (unless degree (error "note not in scale"))
  (define s-ivl (list-ref scale degree))
  (define midi∆ (- (ivl-midi∆ n-ivl) (ivl-midi∆ s-ivl)))
  (define-values [octave alteration]
    (quotient/remainder midi∆ 12))
  (scale-note (length scale) (+ (* N octave) degree) alteration))

;; ------------------------------------------------------------------------

(provide major
         natural-minor)

;; A ScaleKind is a [Listof Interval]
(define major
  (list unison M2nd M3rd P4th P5th M6th M7th))
(define natural-minor
  (list unison M2nd m3rd P4th P5th m6th m7th))

;; ------------------------------------------------------------------------

