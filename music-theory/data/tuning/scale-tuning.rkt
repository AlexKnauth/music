#lang agile

(require "../scale/scale-note.rkt")

(module+ example
  (provide (all-defined-out)))

;; ------------------------------------------------------------------------

(provide scale-tuning
         scale-tuning-number
         scale-tuning-frequency/diatonic
         scale-tuning-frequency/alteration)

;; A ScaleTuning is a (scale-tuning PosReal ScaleKindTuning)
(struct scale-tuning [root-frequency kind] #:transparent)

;; scale-tuning-number : ScaleTuning -> Nat
(define (scale-tuning-number st)
  (scale-kind-tuning-number (scale-tuning-kind st)))

;; scale-tuning-frequency/diatonic : ScaleTuning Int Nat -> PosReal
(define (scale-tuning-frequency/diatonic st octave degree)
  (match st
    [(scale-tuning root kind)
     (* root
        (expt 2 octave)
        (list-ref (scale-kind-tuning-ratios kind) degree))]))

;; scale-tuning-frequency/alteration : ScaleTuning Int Nat Int -> PosReal
(define (scale-tuning-frequency/alteration st octave degree alteration)
  (* (scale-tuning-frequency/diatonic st octave degree)
     ((scale-kind-tuning-alteration (scale-tuning-kind st))
      degree
      alteration)))

;; ------------------------------------------------------------------------

(provide with-scale-tuning
         scale-note->frequency)

;; current-scale-tuning : [Parameterof [Maybe ScaleTuning]]
(define current-scale-tuning (make-parameter #f))

;; call-with-scale-tuning : ScaleTuning [-> X] -> X
(define (call-with-scale-tuning st thunk)
  (parameterize ([current-scale-tuning st])
    (thunk)))

(define-simple-macro (with-scale-tuning st:expr body:expr ...+)
  (call-with-scale-tuning st (λ () body ...)))

;; scale-note->frequency : ScaleNote -> PosReal
(define (scale-note->frequency sn)
  (define st (current-scale-tuning))
  (unless (scale-tuning? st)
    (error 'scale-note->frequency "must be within `with-scale-tuning`"))
  (scale-tuning-frequency/alteration
   st
   (scale-note-octave sn)
   (scale-note-degree sn)
   (scale-note-alteration sn)))

;; ------------------------------------------------------------------------

;; A ScaleKindTuning is a
;; (scale-kind-tuning [Listof PosReal] [Nat Int -> PosReal])
(struct scale-kind-tuning [ratios alteration] #:transparent)

;; scale-kind-tuning-number : ScaleKindTuning -> Nat
(define (scale-kind-tuning-number skt)
  (length (scale-kind-tuning-ratios skt)))

;; frequency-alteration/12TET : Nat Int -> PosReal
(define (frequency-alteration/12TET degree alteration)
  (expt 2 (/ alteration 12)))

;; ------------------------------------------------------------------------

(module+ example

  ;; just-major : ScaleKindTuning
  (define just-major
    (scale-kind-tuning
     (list
      ;; tonic    : fundamental
      1
      ;; tonic    : 9th harmonic
      ;; dominant : 3rd harmonic
      9/8
      ;; tonic    : 5th harmonic
      5/4
      ;; dominant : 7th harmonic
      21/16
      ;; tonic    : 3rd harmonic
      ;; dominant : fundamental
      3/2
      ;; dominant : 9th harmonic
      27/16
      ;; tonic    : 15th harmonic
      ;; dominant : 5th harmonic
      15/8)
     frequency-alteration/12TET))

  ;; 12TET-chromatic : ScaleKindTuning
  (define 12TET-chromatic
    (scale-kind-tuning
     (list
      (expt 2 0/12)
      (expt 2 1/12)
      (expt 2 2/12)
      (expt 2 3/12)
      (expt 2 4/12)
      (expt 2 5/12)
      (expt 2 6/12)
      (expt 2 7/12)
      (expt 2 8/12)
      (expt 2 9/12)
      (expt 2 10/12)
      (expt 2 11/12))
     frequency-alteration/12TET))

  ;; 12TET-major : ScaleKindTuning
  (define 12TET-major
    (scale-kind-tuning
     (list
      (expt 2 0/12)
      (expt 2 2/12)
      (expt 2 4/12)
      (expt 2 5/12)
      (expt 2 7/12)
      (expt 2 9/12)
      (expt 2 11/12))
     frequency-alteration/12TET))

  ;; 12TET-natural-minor : ScaleKindTuning
  (define 12TET-natural-minor
    (scale-kind-tuning
     (list
      (expt 2 0/12)
      (expt 2 2/12)
      (expt 2 3/12)
      (expt 2 5/12)
      (expt 2 7/12)
      (expt 2 8/12)
      (expt 2 10/12))
     frequency-alteration/12TET))

  ;; 7TET : ScaleKindTuning
  (define 7TET
    (scale-kind-tuning
     (list
      (expt 2 0/7)
      (expt 2 1/7)
      (expt 2 2/7)
      (expt 2 3/7)
      (expt 2 4/7)
      (expt 2 5/7)
      (expt 2 6/7))
     frequency-alteration/12TET))

  )

;; ------------------------------------------------------------------------

