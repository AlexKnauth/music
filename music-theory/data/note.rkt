#lang agile

(module+ example
  (provide (all-defined-out))
  (define-simple-macro (defs [x:id e:expr] ...)
    (begin (define x e) ...)))
(module+ test
  (require rackunit
           (submod ".." example)))

;; ------------------------------------------------------------------------

;; Notes

(provide C D E F G A B flat sharp
         note-name-string
         note-octave
         note=? note-midi=?)

(struct note [midi-number name] #:transparent)

;; A Note is a (note MidiNumber NoteName)
;; A MidiNumber is an Int representing one of the midi tones
;; A NoteName is an Int[0,6] representing which name it belongs to
(define (note-name+ a b) (modulo (+ a b) 7))
(define (note-name- a b) (modulo (- a b) 7))

;; note=? : Note Note -> Bool
(define (note=? a b)
  (and (= (note-midi-number a) (note-midi-number b))
       (= (note-name a) (note-name b))))

;; note-midi=? : Note Note -> Bool
(define (note-midi=? a b)
  (= (note-midi-number a) (note-midi-number b)))

(define (C octave) (note (+ 12 (* 12 octave)) 0))
(define (D octave) (note (+ 14 (* 12 octave)) 1))
(define (E octave) (note (+ 16 (* 12 octave)) 2))
(define (F octave) (note (+ 17 (* 12 octave)) 3))
(define (G octave) (note (+ 19 (* 12 octave)) 4))
(define (A octave) (note (+ 21 (* 12 octave)) 5))
(define (B octave) (note (+ 23 (* 12 octave)) 6))

;; flat : Note -> Note
(define (flat n)
  (match n
    [(note midi name)
     (note (sub1 midi) name)]))

;; sharp : Note -> Note
(define (sharp n)
  (match n
    [(note midi name)
     (note (add1 midi) name)]))

;; note-name-string : Note -> String
(define (note-name-string n)
  (match (note-name n)
    [0 "C"]
    [1 "D"]
    [2 "E"]
    [3 "F"]
    [4 "G"]
    [5 "A"]
    [6 "B"]))

;; note-octave : Note -> Int
(define (note-octave n)
  ;; TODO: What should happen with C♭ and B#?
  (match n
    [(note midi _)
     (+ (quotient midi 12) -1)]))

(module+ test
  (check-equal? (A 0) (note 21 5))
  (check-equal? (B 0) (note 23 6))
  (check-equal? (C 1) (note 24 0))
  (check-equal? (B 3) (note 59 6))
  (check-equal? (C 4) (note 60 0))
  (check-equal? (D 4) (note 62 1))

  (check-equal? (sharp (G 4)) (note 68 4))
  (check-equal? (flat (A 4)) (note 68 5))
  (check-false (note=? (sharp (G 4)) (flat (A 4))))
  (check-true (note-midi=? (sharp (G 4)) (flat (A 4)))))

(module+ example
  (defs
    [C3 (C 3)] [C4 (C 4)] [C5 (C 5)]
    [D3 (D 3)] [D4 (D 4)] [D5 (D 5)]
    [E3 (E 3)] [E4 (E 4)] [E5 (E 5)]
    [F3 (F 3)] [F4 (F 4)] [F5 (F 5)]
    [G3 (G 3)] [G4 (G 4)] [G5 (G 5)]
    [A3 (A 3)] [A4 (A 4)] [A5 (A 5)]
    [B3 (B 3)] [B4 (B 4)] [B5 (B 5)])

  (defs
    [F#4 (sharp F4)] [F#5 (sharp F5)]
    [C#4 (sharp C4)] [C#5 (sharp C5)]
    [G#4 (sharp G4)] [G#5 (sharp G5)]
    [D#4 (sharp D4)] [D#5 (sharp D5)]
    [A#4 (sharp A4)] [A#5 (sharp A5)])

  (defs
    [B♭4 (flat B4)] [B♭5 (flat B5)]
    [E♭4 (flat E4)] [E♭5 (flat E5)]
    [A♭4 (flat A4)] [A♭5 (flat A5)]
    [D♭4 (flat D4)] [D♭5 (flat D5)]
    [G♭4 (flat G4)] [G♭5 (flat G5)]))

;; ------------------------------------------------------------------------

;; Intervals

(provide unison m2nd M2nd m3rd M3rd P4th d5th P5th m6th M6th m7th M7th
         octave d7th
         ivl=? ivl-midi=? note+ ivl+ note∆)

(struct interval [midi∆ name∆] #:transparent)

;; An Internal is an (interval Int Int) representing a distance between Notes

;; ivl=? : Interval Interval -> Bool
(define (ivl=? a b)
  (and (= (interval-midi∆ a) (interval-midi∆ b))
       (= (interval-name∆ a) (interval-name∆ b))))

;; ivl-midi=? : Interval Interval -> Bool
(define (ivl-midi=? a b)
  (= (interval-midi∆ a) (interval-midi∆ b)))

(define unison (interval 0 0))
(define m2nd (interval 1 1))
(define M2nd (interval 2 1))
(define m3rd (interval 3 2))
(define M3rd (interval 4 2))
(define P4th (interval 5 3))
(define A4th (interval 6 3))
(define d5th (interval 6 4))
(define P5th (interval 7 4))
(define A5th (interval 8 4))
(define m6th (interval 8 5))
(define M6th (interval 9 5))
(define d7th (interval 9 6))
(define m7th (interval 10 6))
(define M7th (interval 11 6))
(define octave (interval 12 0))

;; note+ : Note Interval -> Note
(define (note+ n i)
  (match* [n i]
    [[(note n nn) (interval i in)]
     (note (+ n i) (note-name+ nn in))]))

;; ivl+ : Interval Interval -> Interval
(define (ivl+ a b)
  (match* [a b]
    [[(interval a an) (interval b bn)]
     (interval (+ a b) (note-name+ an bn))]))

;; note∆ : Note Note -> Interval
(define (note∆ a b)
  (match* [a b]
    [[(note a an) (note b bn)]
     (interval (- b a) (note-name- bn an))]))

(module+ test
  (define-check (check-note∆ a b ∆)
    (check-equal? (note+ a ∆) b)
    (check-equal? (note∆ a b) ∆))

  (check-note∆ C4 C4 unison)
  (check-note∆ C4 D4 M2nd)
  (check-note∆ C4 E4 M3rd)
  (check-note∆ C4 F4 P4th)
  (check-note∆ C4 G4 P5th)
  (check-note∆ C4 A4 M6th)
  (check-note∆ C4 B4 M7th)
  (check-note∆ C4 C5 octave)
  (check-note∆ D4 F4 m3rd)
  (check-note∆ B3 F4 d5th)
  (check-note∆ F4 B4 A4th)
  (check-note∆ A3 F4 m6th))

;; ------------------------------------------------------------------------

