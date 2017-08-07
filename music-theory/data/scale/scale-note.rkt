#lang agile

(require "../note/note.rkt"
         "scale.rkt")
(module+ example
  (provide (all-defined-out))
  (require music-theory/util/defs))

;; ------------------------------------------------------------------------

(provide scale-note
         scale-note?
         scale-note-diatonic
         scale-note-octave
         scale-note-degree
         scale-note-alteration
         scale-note-octave+
         scale-note-alteration+)

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

;; scale-note-octave+ : ScaleNote Int -> ScaleNote
(define (scale-note-octave+ sn i)
  (match sn
    [(scale-note d a)
     (scale-note (diatonic-octave+ d i) a)]))

(define (diatonic-octave+ d i)
  (match d
    [(diatonic o d)
     (diatonic (+ o i) d)]))

;; scale-note-alteration+ : ScaleNote Int -> ScaleNote
(define (scale-note-alteration+ sn i)
  (match sn
    [(scale-note d a)
     (scale-note d (+ a i))]))

(define (scale-note-sharp sn)
  (scale-note-alteration+ sn +1))

(define (scale-note-flat sn)
  (scale-note-alteration+ sn -1))

(module+ example
  (define (nat0 n)
    (scale-note (diatonic 0 n) 0))
  (define (nat1 n)
    (scale-note (diatonic 1 n) 0))
  (define (nat2 n)
    (scale-note (diatonic 2 n) 0))
  (define (nat3 n)
    (scale-note (diatonic 3 n) 0))
  (define (nat4 n)
    (scale-note (diatonic 4 n) 0))

  (defs/f nat0
    [s0:0 0] [s1:0 1] [s2:0 2] [s3:0 3] [s4:0 4] [s5:0 5] [s6:0 6])

  (defs/f nat1
    [s0:1 0] [s1:1 1] [s2:1 2] [s3:1 3] [s4:1 4] [s5:1 5] [s6:1 6])

  (defs/f nat2
    [s0:2 0] [s1:2 1] [s2:2 2] [s3:2 3] [s4:2 4] [s5:2 5] [s6:2 6])

  (defs/f nat3
    [s0:3 0] [s1:3 1] [s2:3 2] [s3:3 3] [s4:3 4] [s5:3 5] [s6:3 6])

  (defs/f nat4
    [s0:4 0] [s1:4 1] [s2:4 2] [s3:4 3] [s4:4 4] [s5:4 5] [s6:4 6])

  (defs/f scale-note-sharp
    [s3#:3 s3:3])

  (defs/f scale-note-flat
    [s6♭:2 s6:2])
  )

;; ------------------------------------------------------------------------

(provide diatonic->number
         number->diatonic
         diatonic-invert/around)

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

;; diatonic-invert/around : Diatonic Diatonic -> Diatonic
(define (diatonic-invert/around d z)
  (define dn (diatonic->number d))
  (define zn (diatonic->number z))
  (define ∆ (- dn zn))
  (number->diatonic (+ zn (- ∆))))

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

