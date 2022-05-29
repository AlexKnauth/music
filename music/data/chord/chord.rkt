#lang agile

(require "../note/note.rkt"
         (prefix-in nc/ "../note/note-class.rkt"))
(module+ test
  (require rackunit
           (submod "../note/note.rkt" example)))

;; ------------------------------------------------------------------------

;; Kinds of Chords

(provide open-power
         major-triad
         minor-triad
         augmented-triad
         diminished-triad
         sus-2
         sus-4
         major-add-6
         minor-add-6
         major-7
         minor-7
         dominant-7
         diminished-7
         minor-7-♭5

         half-diminished-7)

;; A ChordKind is a [Listof Interval]
;; Should be sorted from least to greatest

;; add : ChordKind Interval ... -> ChordKind
;; Each new interval should be sorted above the intervals in kind
(define (add kind . new) (append kind new))

(define open-power (list unison P5th))
(define major-triad (list unison M3rd P5th))
(define minor-triad (list unison m3rd P5th))
(define augmented-triad (list unison M3rd A5th))
(define diminished-triad (list unison m3rd d5th))
(define sus-2 (list unison M2nd P5th))
(define sus-4 (list unison P4th P5th))

(define major-add-6 (add major-triad M6th))
(define minor-add-6 (add minor-triad M6th))
(define major-7 (add major-triad M7th))
(define minor-7 (add minor-triad m7th))
(define dominant-7 (add major-triad m7th))
(define diminished-7 (add diminished-triad d7th))
(define minor-7-♭5 (add diminished-triad m7th))

;; aliases
(define half-diminished-7 minor-7-♭5)

;; ------------------------------------------------------------------------

;; Building Chords from a given Root

(provide chord chord=? chord-midi=?
         chord-octave+
         chord-approx/bass+class=?)

;; A Chord is a [Listof Note]

;; chord : Note ChordKind -> Chord
(define (chord root kind)
  (for/list ([ivl (in-list kind)])
    (note+ root ivl)))

;; chord=? : Chord Chord -> Bool
(define (chord=? a b)
  (and (= (length a) (length b))
       (andmap note=? a b)))

;; chord-midi=? : Chord Chord -> Bool
(define (chord-midi=? a b)
  (and (= (length a) (length b))
       (andmap note-midi=? a b)))

;; chord-octave+ : Chord Int -> Chord
(define (chord-octave+ chord i)
  (for/list ([n (in-list chord)])
    (note-octave+ n i)))

;; chord-approx/bass+class=? : Chord Chord -> Bool
(define (chord-approx/bass+class=? a b)
  (cond
    [(and (empty? a) (empty? b)) #true]
    [else
     (define ac (map note-class a))
     (define bc (map note-class b))
     (and
      (note=? (argmin note-midi-number a)
              (argmin note-midi-number b))
      (for/and ([a (in-list ac)])
        (for/or ([b (in-list bc)])
          (nc/note-class=? a b)))
      (for/and ([b (in-list bc)])
        (for/or ([a (in-list ac)])
          (nc/note-class=? a b))))]))

(module+ test
  (test-case "chord building"
    (check-equal? (chord C4 major-triad) (list C4 E4 G4))
    (check-equal? (chord C4 minor-triad) (list C4 E♭4 G4))
    (check-equal? (chord C4 diminished-triad) (list C4 E♭4 G♭4))
    (check-equal? (chord C4 major-7) (list C4 E4 G4 B4))
    (check-equal? (chord C4 dominant-7) (list C4 E4 G4 B♭4))
    (check-equal? (chord C4 minor-7) (list C4 E♭4 G4 B♭4))
    (check-equal? (chord C4 minor-7-♭5) (list C4 E♭4 G♭4 B♭4))
    (check-equal? (chord G3 major-triad) (list G3 B3 D4))
    (check-equal? (chord G3 dominant-7) (list G3 B3 D4 F4))
    (check-equal? (chord B3 diminished-triad) (list B3 D4 F4))
    (check-equal? (chord B3 diminished-7) (list B3 D4 F4 A♭4))

    (check-equal? (chord F#4 major-triad) (list F#4 A#4 C#5))
    (check-equal? (chord G♭4 major-triad) (list G♭4 B♭4 D♭5))
    (check-false (chord=? (chord F#4 major-triad)
                          (chord G♭4 major-triad)))
    (check-true (chord-midi=? (chord F#4 major-triad)
                              (chord G♭4 major-triad)))))

;; ------------------------------------------------------------------------

;; Inversions

(provide inversion)

;; An InversionNumber is a Nat
;;   0 = root-position
;;   1 = first-inversion
;;   2 = second-inversion
;;   3 = third-inversion

;; inversion : ChordKind InversionNumber -> ChordKind
(define (inversion kind inversion)
  (define (ivls k i acc)
    (match k
      ['()
       (error 'inversion
              "inversion ~a does not exist for chord kind ~a"
              inversion kind)]
      [(cons fst rst)
       (cond
         [(zero? i) (append k (reverse acc))]
         [else (ivls rst (sub1 i) (cons (ivl+ fst octave) acc))])]))
  (cond
    [(zero? inversion) kind]
    [else (ivls kind inversion '())]))

(module+ test
  (test-case "chord building with inversions"
    (check-equal? (chord C4 (inversion major-triad 0)) (list C4 E4 G4))
    (check-equal? (chord C4 (inversion major-triad 1)) (list E4 G4 C5))
    (check-equal? (chord C4 (inversion major-triad 2)) (list G4 C5 E5))
    (check-equal? (chord G3 (inversion dominant-7 0)) (list G3 B3 D4 F4))
    (check-equal? (chord G3 (inversion dominant-7 1)) (list B3 D4 F4 G4))
    (check-equal? (chord G3 (inversion dominant-7 2)) (list D4 F4 G4 B4))
    (check-equal? (chord G3 (inversion dominant-7 3)) (list F4 G4 B4 D5))))

;; ------------------------------------------------------------------------

