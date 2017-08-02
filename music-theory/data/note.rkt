#lang agile

(require (prefix-in nc/ "note-class.rkt"))
(module+ example
  (provide (all-defined-out))
  (require "../util/defs.rkt"))
(module+ test
  (require rackunit
           (submod ".." example)))

;; ------------------------------------------------------------------------

;; Notes

(provide C D E F G A B flat sharp
         note-midi-number
         note-name-string
         note-alteration
         note=? note-midi=?
         note-midi<? note-midi<=?
         note-alteration+)

(struct note [midi-number name] #:transparent)

;; A Note is a (note MidiNumber NoteName)
;; A MidiNumber is an Int representing one of the midi tones
;; A NoteName is an Int representing which name it belongs to

;; note=? : Note Note -> Bool
(define (note=? a b)
  (and (= (note-midi-number a) (note-midi-number b))
       (= (note-name a) (note-name b))))

(define-simple-macro (midi-binary-predicate op:id)
  ;; Note Note -> Bool
  (λ (a b)
    (op (note-midi-number a) (note-midi-number b))))

;; note-midi=? : Note Note -> Bool
(define note-midi=? (midi-binary-predicate =))

;; note-midi<? : Note Note -> Bool
(define note-midi<? (midi-binary-predicate <))

;; note-midi<=? : Note Note -> Bool
(define note-midi<=? (midi-binary-predicate <=))

(define (note/name name)
  (note (name->midi name) name))

(define (C octave) (note/name (+ 0 (* 7 octave))))
(define (D octave) (note/name (+ 1 (* 7 octave))))
(define (E octave) (note/name (+ 2 (* 7 octave))))
(define (F octave) (note/name (+ 3 (* 7 octave))))
(define (G octave) (note/name (+ 4 (* 7 octave))))
(define (A octave) (note/name (+ 5 (* 7 octave))))
(define (B octave) (note/name (+ 6 (* 7 octave))))

;; flat : Note -> Note
(define (flat n)
  (note-alteration+ n -1))

;; sharp : Note -> Note
(define (sharp n)
  (note-alteration+ n +1))

;; note-alteration : Note -> Int
(define (note-alteration n)
  (match n
    [(note midi name)
     (- midi (name->midi name))]))

;; note-alteration+ : Note Int -> Note
(define (note-alteration+ n i)
  (match n
    [(note midi name)
     (note (+ midi i) name)]))

(module+ test
  (check-equal? (A 0) (note 21 5))
  (check-equal? (B 0) (note 23 6))
  (check-equal? (C 1) (note 24 (+ 0 7)))
  (check-equal? (B 3) (note 59 (+ 6 21)))
  (check-equal? (C 4) (note 60 (+ 0 28)))
  (check-equal? (D 4) (note 62 (+ 1 28)))

  (check-equal? (sharp (G 4)) (note 68 (+ 4 28)))
  (check-equal? (flat (A 4)) (note 68 (+ 5 28)))
  (check-false (note=? (sharp (G 4)) (flat (A 4))))
  (check-true (note-midi=? (sharp (G 4)) (flat (A 4)))))

;; ------------------------------------------------------------------------

(module+ example
  (defs/f C [C1 1] [C2 2] [C3 3] [C4 4] [C5 5])
  (defs/f D [D1 1] [D2 2] [D3 3] [D4 4] [D5 5])
  (defs/f E [E1 1] [E2 2] [E3 3] [E4 4] [E5 5])
  (defs/f F [F1 1] [F2 2] [F3 3] [F4 4] [F5 5])
  (defs/f G [G1 1] [G2 2] [G3 3] [G4 4] [G5 5])
  (defs/f A [A1 1] [A2 2] [A3 3] [A4 4] [A5 5])
  (defs/f B [B1 1] [B2 2] [B3 3] [B4 4] [B5 5])

  (defs/f sharp
    [F#1 F1] [F#2 F2] [F#3 F3] [F#4 F4] [F#5 F5]
    [C#1 C1] [C#2 C2] [C#3 C3] [C#4 C4] [C#5 C5]
    [G#1 G1] [G#2 G2] [G#3 G3] [G#4 G4] [G#5 G5]
    [D#1 D1] [D#2 D2] [D#3 D3] [D#4 D4] [D#5 D5]
    [A#1 A1] [A#2 A2] [A#3 A3] [A#4 A4] [A#5 A5]
    [E#1 E1] [E#2 E2] [E#3 E3] [E#4 E4] [E#5 E5]
    [B#1 B1] [B#2 B2] [B#3 B3] [B#4 B4] [B#5 B5])

  (defs/f flat
    [B♭1 B1] [B♭2 B2] [B♭3 B3] [B♭4 B4] [B♭5 B5]
    [E♭1 E1] [E♭2 E2] [E♭3 E3] [E♭4 E4] [E♭5 E5]
    [A♭1 A1] [A♭2 A2] [A♭3 A3] [A♭4 A4] [A♭5 A5]
    [D♭1 D1] [D♭2 D2] [D♭3 D3] [D♭4 D4] [D♭5 D5]
    [G♭1 G1] [G♭2 G2] [G♭3 G3] [G♭4 G4] [G♭5 G5]
    [C♭1 C1] [C♭2 C2] [C♭3 C3] [C♭4 C4] [C♭5 C5]
    [F♭1 F1] [F♭2 F2] [F♭3 F3] [F♭4 F4] [F♭5 F5]))

;; ------------------------------------------------------------------------

;; Note Classes and Octaves

(provide note-octave
         note-octave+
         note-class
         note-class=?
         note->string)

;; Any accidentals follow the octave designation of the natural pitch with
;; the same generic name. Thus a half step below C4 is C♭4 (even though
;; it sounds the same as B3), and a half step above B4 is B#4 (even though
;; it sounds the same as C5).

;; note-octave : Note -> Int
(define (note-octave n)
  (match n
    [(note midi name)
     (define name/7 (modulo name 7))
     (/ (- name name/7) 7)]))

;; note-octave+ : Note Int -> Note
(define (note-octave+ n i)
  (match n
    [(note midi name)
     (note (+ midi (* 12 i)) (+ name (* 7 i)))]))

;; note-class : Note -> NoteClass
(define (note-class n)
  (match n
    [(note midi name)
     (nc/note-class (modulo midi 12) (modulo name 7))]))

;; note/class : NoteClass Int -> Note
(define (note/class nc octave)
  (match nc
    [(nc/note-class pc name-class)
     (define name (+ name-class (* 7 octave)))
     (define name-midi
       (name->midi name))
     (define alteration
       (nc/note-class-alteration nc))
     (note (+ name-midi alteration) name)]))

;; note-class=? : Note Note -> Bool
(define (note-class=? a b)
  (nc/note-class=? (note-class a) (note-class b)))

;; note->string : Note -> String
(define (note->string n)
  (string-append
   (note-name-string n)
   (note-alteration-string n)
   (number->string (note-octave n))))

;; note-name-string : Note -> String
(define (note-name-string n)
  (nc/name-class-string (modulo (note-name n) 7)))

;; note-alteration-string : Note -> String
(define (note-alteration-string n)
  (nc/alteration-string (note-alteration n)))

(module+ test
  (check-equal? (note-octave C1) 1)
  (check-equal? (note-octave C2) 2)
  (check-equal? (note-octave C3) 3)
  (check-equal? (note-octave C4) 4)
  (check-equal? (note-octave C5) 5)

  (check-equal? (note-octave C♭2) 2)
  (check-equal? (note-octave B1) 1)
  (check-equal? (note-octave B#1) 1)
  (check-equal? (note-octave C2) 2)

  (define-check (check-note-class n c o)
    (check-equal? (note-class n) c)
    (check-equal? (note-octave n) o)
    (check-equal? (note/class c o) n)
    (check-equal? (note-alteration n) (nc/note-class-alteration c)
                  "alteration"))

  (check-note-class C♭2 nc/C♭ 2)
  (check-note-class C2  nc/C 2)
  (check-note-class C#2 nc/C# 2)
  (check-note-class D♭2 nc/D♭ 2)
  (check-note-class D2  nc/D 2)
  (check-note-class D#2 nc/D# 2)
  (check-note-class E♭2 nc/E♭ 2)
  (check-note-class E2  nc/E 2)
  (check-note-class E#2 nc/E# 2)
  (check-note-class F♭2 nc/F♭ 2)
  (check-note-class F2  nc/F 2)
  (check-note-class F#2 nc/F# 2)
  (check-note-class G♭2 nc/G♭ 2)
  (check-note-class G2  nc/G 2)
  (check-note-class G#2 nc/G# 2)
  (check-note-class A♭2 nc/A♭ 2)
  (check-note-class A2  nc/A 2)
  (check-note-class A#2 nc/A# 2)
  (check-note-class B♭2 nc/B♭ 2)
  (check-note-class B2  nc/B 2)
  (check-note-class B#2 nc/B# 2)

  (check-note-class C♭3 nc/C♭ 3)
  (check-note-class C3  nc/C 3)
  (check-note-class C#3 nc/C# 3)
  (check-note-class D♭3 nc/D♭ 3)
  (check-note-class D3  nc/D 3)
  (check-note-class D#3 nc/D# 3)
  (check-note-class E♭3 nc/E♭ 3)
  (check-note-class E3  nc/E 3)
  (check-note-class E#3 nc/E# 3)
  (check-note-class F♭3 nc/F♭ 3)
  (check-note-class F3  nc/F 3)
  (check-note-class F#3 nc/F# 3)
  (check-note-class G♭3 nc/G♭ 3)
  (check-note-class G3  nc/G 3)
  (check-note-class G#3 nc/G# 3)
  (check-note-class A♭3 nc/A♭ 3)
  (check-note-class A3  nc/A 3)
  (check-note-class A#3 nc/A# 3)
  (check-note-class B♭3 nc/B♭ 3)
  (check-note-class B3  nc/B 3)
  (check-note-class B#3 nc/B# 3)

  (check-equal? (note->string C#4) "C#4")
  (check-equal? (note->string F♭2) "F♭2")
  (check-equal? (note->string B#3) "B#3")
  (check-equal? (note->string C♭5) "C♭5"))

;; ------------------------------------------------------------------------

(define (name->midi name)
  (define name/7 (modulo name 7))
  (define octave (/ (- name name/7) 7))
  (match name/7
    [0 (+ 12 (* 12 octave))]
    [1 (+ 14 (* 12 octave))]
    [2 (+ 16 (* 12 octave))]
    [3 (+ 17 (* 12 octave))]
    [4 (+ 19 (* 12 octave))]
    [5 (+ 21 (* 12 octave))]
    [6 (+ 23 (* 12 octave))]))

;; ------------------------------------------------------------------------

;; Intervals

(provide unison ivl-sharp
         m2nd M2nd
         m3rd M3rd
         P4th A4th
         d5th P5th A5th
         m6th M6th
         d7th m7th M7th
         octave
         ivl=? ivl-midi=? ivl-name∆/7=?
         ivl-midi<?
         ivl-midi-zero?
         ivl-midi-positive?
         note∆ ivl-midi∆
         note+ ivl+)

(struct interval [midi∆ name∆] #:transparent)

;; An Internal is an (interval Int Int) representing a distance between Notes

;; ivl=? : Interval Interval -> Bool
(define (ivl=? a b)
  (and (= (interval-midi∆ a) (interval-midi∆ b))
       (= (interval-name∆ a) (interval-name∆ b))))

(define-simple-macro (ivl-midi-binary-predicate op:id)
  ;; Interval Interval -> Bool
  (λ (a b)
    (op (interval-midi∆ a) (interval-midi∆ b))))

;; ivl-midi=? : Interval Interval -> Bool
(define ivl-midi=? (ivl-midi-binary-predicate =))

;; ivl-midi<? : Interval Interval -> Bool
(define ivl-midi<? (ivl-midi-binary-predicate <))

;; ivl-midi-zero? : Interval -> Bool
(define (ivl-midi-zero? a) (zero? (interval-midi∆ a)))

;; ivl-midi-positive? : Interval -> Bool
(define (ivl-midi-positive? a) (positive? (interval-midi∆ a)))

;; ivl-name∆/7=? : Interval Interval -> Bool
(define (ivl-name∆/7=? a b)
  (= (modulo (interval-name∆ a) 7)
     (modulo (interval-name∆ b) 7)))

;; ivl-octaves-apart? : Interval -> Bool
(define (ivl-octaves-apart? a)
  (and (zero? (modulo (interval-midi∆ a) 12))
       (zero? (modulo (interval-name∆ a) 7))))

;; ivl-midi∆ : Interval -> Int
(define (ivl-midi∆ a)
  (interval-midi∆ a))

(define unison (interval 0 0))
(define ivl-sharp (interval 1 0))
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
(define octave (interval 12 7))

;; note+ : Note Interval -> Note
(define (note+ n i)
  (match* [n i]
    [[(note n nn) (interval i in)]
     (note (+ n i) (+ nn in))]))

;; ivl+ : Interval Interval -> Interval
(define (ivl+ a b)
  (match* [a b]
    [[(interval a an) (interval b bn)]
     (interval (+ a b) (+ an bn))]))

;; note∆ : Note Note -> Interval
(define (note∆ a b)
  (match* [a b]
    [[(note a an) (note b bn)]
     (interval (- b a) (- bn an))]))

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

