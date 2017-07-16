#lang agile

(require "../note.rkt")
(module+ test
  (require rackunit
           (submod "../note.rkt" example)))

;; ------------------------------------------------------------------------

(provide note-pitch-class)

;; A PitchClass is an Int[0,11]

;; note-pitch-class : Note -> PitchClass
(define (note-pitch-class n)
  (modulo (note-midi-number n) 12))

(module+ test
  (check-equal? (note-pitch-class C4) 0)
  (check-equal? (note-pitch-class C#4) 1)
  (check-equal? (note-pitch-class D♭4) 1)
  (check-equal? (note-pitch-class D4) 2)
  (check-equal? (note-pitch-class D#4) 3)
  (check-equal? (note-pitch-class E♭4) 3)
  (check-equal? (note-pitch-class E4) 4)
  (check-equal? (note-pitch-class F4) 5)
  (check-equal? (note-pitch-class F#4) 6)
  (check-equal? (note-pitch-class G♭4) 6)
  (check-equal? (note-pitch-class G4) 7)
  (check-equal? (note-pitch-class G#4) 8)
  (check-equal? (note-pitch-class A♭4) 8)
  (check-equal? (note-pitch-class A4) 9)
  (check-equal? (note-pitch-class A#4) 10)
  (check-equal? (note-pitch-class B♭4) 10)
  (check-equal? (note-pitch-class B4) 11)

  (check-equal? (note-pitch-class C5) 0)
  (check-equal? (note-pitch-class C#5) 1)
  (check-equal? (note-pitch-class D♭5) 1)
  (check-equal? (note-pitch-class D5) 2)
  (check-equal? (note-pitch-class D#5) 3)
  (check-equal? (note-pitch-class E♭5) 3)
  (check-equal? (note-pitch-class E5) 4)
  (check-equal? (note-pitch-class F5) 5)
  (check-equal? (note-pitch-class F#5) 6)
  (check-equal? (note-pitch-class G♭5) 6)
  (check-equal? (note-pitch-class G5) 7)
  (check-equal? (note-pitch-class G#5) 8)
  (check-equal? (note-pitch-class A♭5) 8)
  (check-equal? (note-pitch-class A5) 9)
  (check-equal? (note-pitch-class A#5) 10)
  (check-equal? (note-pitch-class B♭5) 10)
  (check-equal? (note-pitch-class B5) 11)
  )

;; ------------------------------------------------------------------------

