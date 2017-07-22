#lang agile

(require "../note.rkt")

;; ------------------------------------------------------------------------

(provide scale
         with-scale)

;; A Scale is a (scale Note ScaleKind)
(struct scale [root kind] #:transparent)

;; current-scale : [Parameterof [Maybe Scale]]
(define current-scale (make-parameter #f))

;; call-with-scale : Scale [-> X] -> X
(define (call-with-scale s f)
  (parameterize ([current-scale s]) (f)))

(define-simple-macro (with-scale s:expr body:expr ...+)
  (call-with-scale s (λ () body ...)))

;; ------------------------------------------------------------------------

(provide major
         natural-minor)

;; A ScaleKind is a [Listof Interval]
(define major
  (list unison M2nd M3rd P4th P5th M6th M7th))
(define natural-minor
  (list unison M2nd m3rd P4th P5th m6th m7th))

;; ------------------------------------------------------------------------

(provide scale-note
         scale-note-diatonic
         scale-note-octave
         scale-note-degree
         scale-note-alteration)

;; A ScaleNote is a (scale-note Diatonic Int)
(struct scale-note [diatonic alteration] #:transparent)

;; A Diatonic is a (diatonic Int Nat)
(struct diatonic [octave degree] #:transparent)

;; scale-note-octave : ScaleNote -> Int
(define (scale-note-octave sn)
  (diatonic-octave (scale-note-diatonic sn)))

;; scale-note-degree : ScaleNote -> Nat
(define (scale-note-degree sn)
  (diatonic-degree (scale-note-diatonic sn)))

;; ------------------------------------------------------------------------

(provide diatonic->number
         number->diatonic)

;; diatonic->number : Diatonic -> Int
(define (diatonic->number d)
  (match-define (scale root kind) (current-scale))
  (match-define (diatonic octave degree) d)
  (+ (* (length kind) octave) degree))

;; number->diatonic : Int -> Diatonic
(define (number->diatonic i)
  (match-define (scale root kind) (current-scale))
  (define N (length kind))
  (define degree (modulo i N))
  (define octave (/ (- i degree) N))
  (diatonic octave degree))

;; ------------------------------------------------------------------------

(provide scale-note->note
         note->scale-note)

;; scale-note->note : ScaleNote -> Note
(define (scale-note->note sn)
  (match-define (scale root kind) (current-scale))
  (match sn
    [(scale-note (diatonic octave degree) alteration)
     (note-alteration+
      (note-octave+
       (note+ root (list-ref kind degree))
       octave)
      alteration)]))

;; note->scale-note : Note -> ScaleNote
(define (note->scale-note n)
  (match-define (scale root kind) (current-scale))
  (define N (length kind))
  (define n-ivl (note∆ root n))
  (define degree
    (for/first ([s-ivl (in-list kind)]
                [i (in-naturals)]
                #:when (ivl-name∆/7=? n-ivl s-ivl))
      i))
  (unless degree (error "note not in scale"))
  (define s-ivl (list-ref kind degree))
  (define midi∆ (- (ivl-midi∆ n-ivl) (ivl-midi∆ s-ivl)))
  (define-values [octave alteration]
    (quotient/remainder midi∆ 12))
  (scale-note (diatonic octave degree) alteration))

;; ------------------------------------------------------------------------

