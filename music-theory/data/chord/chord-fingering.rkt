#lang agile

(require "../note.rkt"
         "chord.rkt")
(module+ example
  (provide (all-defined-out)))
(module+ test
  (require rackunit
           (submod ".." example)
           (submod "../note.rkt" example)))

;; ------------------------------------------------------------------------

;; See the JGuitar Chord Calculator:
;; http://jguitar.com/chord

;; ------------------------------------------------------------------------

(provide violin-strings
         viola-strings
         cello-strings
         bass-strings
         guitar-strings)

;; A StringSpec is a [Listof Note]
;; Representing the notes that the open strings would play

(define violin-strings
  (list (G 3) (D 4) (A 4) (E 5)))

(define viola-strings
  (list (C 3) (G 3) (D 4) (A 4)))

(define cello-strings
  (list (C 2) (G 2) (D 3) (A 3)))

;; both string base and bass guitar
(define bass-strings
  (list (E 1) (A 1) (D 2) (G 2)))

(define guitar-strings
  (list (E 2) (A 2) (D 3) (G 3) (B 3) (E 4)))

;; ------------------------------------------------------------------------

(provide chord-layout->chord)

;; ChordLayout is a [Listof [Maybe Interval]]
;; For a given StringSpec, each interval represents the interval of the
;; note above the string.

;; chord-layout->chord : StringSpec ChordLayout -> Chord
(define (chord-layout->chord strings layout)
  (for/list ([string (in-list strings)]
             [ivl (in-list layout)]
             #:when ivl)
    (note+ string ivl)))

(module+ example

  ;; ---------------------------------------------

  ;; Standard Major Chords

  (define guitar-standard-F
    (list #f #f m3rd M2nd m2nd m2nd))

  (define guitar-standard-C
    (list #f m3rd M2nd unison m2nd unison))

  (define guitar-standard-G
    (list m3rd M2nd unison unison unison m3rd))

  (define guitar-standard-D
    (list #f #f unison M2nd m3rd M2nd))

  (define guitar-standard-A
    (list #f unison M2nd M2nd M2nd unison))

  (define guitar-standard-E
    (list unison M2nd M2nd ivl-sharp unison unison))

  ;; ---------------------------------------------

  ;; Standard Minor Chords

  (define guitar-standard-Dm
    (list #f #f unison M2nd m3rd m2nd))

  (define guitar-standard-Am
    (list #f unison M2nd M2nd m2nd unison))

  (define guitar-standard-Em
    (list unison M2nd M2nd unison unison unison))

  (define guitar-standard-Bm
    (list #f M2nd M3rd M3rd m3rd M2nd))

  ;; ---------------------------------------------

  )

(module+ test
  (define (chord-approx? actual expected)
    (define n (min 1 (length expected)))
    (and (<= n (length actual))
         (chord=? (take actual n) (take expected n))
         (for/and ([a (in-list actual)])
           (for/or ([b (in-list expected)])
             (note-octaves-apart? a b)))
         (for/and ([b (in-list expected)])
           (for/or ([a (in-list actual)])
             (note-octaves-apart? a b)))))

  (define-binary-check (check-chord actual expected)
    (chord-approx? actual expected))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-F)
               (chord F3 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-C)
               (chord C3 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-G)
               (chord G2 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-D)
               (chord D3 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-A)
               (chord A2 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-E)
               (chord E2 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-Dm)
               (chord D3 minor-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-Am)
               (chord A2 minor-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-Em)
               (chord E2 minor-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-Bm)
               (chord B2 minor-triad))

  )

;; ------------------------------------------------------------------------

;; Generating Chord Layouts

(provide generate-chord-layouts)

;; generate-chord-layouts : StringSpec Chord -> [Listof ChordLayout]
;; The chord must be sorted from lowest note to highest
(define (generate-chord-layouts strings chord)
  (reverse (generate-chord-layouts/acc strings chord 0 '())))

;; generate-chord-layouts/acc :
;; StringSpec Chord Nat [Listof ChordLayout] -> [Listof ChordLayout]
(define (generate-chord-layouts/acc strings chord n acc)
  ;; try it starting from every string
  (cond
    [(empty? strings) acc]
    [else
     (generate-chord-layouts/acc
      (rest strings)
      chord
      (add1 n)
      (generate-chord-layouts-from strings chord n acc))]))

;; generate-chord-layouts-from :
;; StringSpec Chord Nat [Listof ChordLayout] -> [Listof ChordLayout]
;; ASSUME strings is not empty
(define (generate-chord-layouts-from strings chord n acc)
  (cond
    [(note-midi<? (first chord) (first strings))
     acc]
    [else
     (continue-chord-layouts (rest strings) chord
       (cons (note∆ (first strings) (first chord))
             (make-list n #false))
       acc)]))

;; continue-chord-layouts :
;; StringSpec Chord [Listof Note] [Listof ChordLayout]
;; -> [Listof ChordLayout]
(define (continue-chord-layouts strings chord acc1 acc2)
  (match strings
    ['()
     (cons (reverse acc1) acc2)]
    [(cons fst rst)
     (for/fold ([acc2 acc2])
               ([next (in-list chord)])
       (continue-chord-layouts rst chord
         (cons (note-put-above∆ fst next) acc1)
         acc2))]))

;; note-put-above∆ : Note Note -> Interval
(define (note-put-above∆ base note)
  (cond [(note-midi<=? base note) (note∆ base note)]
        [else (note-put-above∆ base (note+ note octave))]))

;; ------------------------------------------------------------------------

(module+ test
  (define-binary-check (check-element actual-list expected-member)
    (member expected-member actual-list))

  (check-element (generate-chord-layouts guitar-strings
                                         (chord E2 major-triad))
                 guitar-standard-E)

  (check-element (generate-chord-layouts guitar-strings
                                         (chord A2 major-triad))
                 guitar-standard-A)

  (check-element (generate-chord-layouts guitar-strings
                                         (chord D3 major-triad))
                 guitar-standard-D)

  (check-element (generate-chord-layouts guitar-strings
                                         (chord G2 major-triad))
                 guitar-standard-G)

  (check-element (generate-chord-layouts guitar-strings
                                         (chord C3 major-triad))
                 guitar-standard-C)

  (check-element (generate-chord-layouts guitar-strings
                                         (chord F3 major-triad))
                 guitar-standard-F)
  )

;; ------------------------------------------------------------------------

#|

;; TODO:
;;   Figure out where this is useful.
;;   Can simpler rules do just as well?

(provide finger finger=?)

;; A Finger is a (finger Hand FingerNum)
;; A FingerNum is a Int[0,4]
;; thumb   : 0
;; pointer : 1
;; middle  : 2
;; ring    : 3
;; pinkie  : 4
;; A Hand is an Int[0,1]
;; left  : 0
;; right : 1

(struct finger [hand num] #:transparent)

;; finger=? : Finger Finger -> Bool
(define (finger=? a b)
  (and (= (finger-hand a) (finger-hand b))
       (= (finger-num a) (finger-num b))))

(module+ example
  ;; left hand
  (define L0 (finger 0 0))
  (define L1 (finger 0 1))
  (define L2 (finger 0 2))
  (define L3 (finger 0 3))
  (define L4 (finger 0 4))

  ;; right hand
  (define R0 (finger 1 0))
  (define R1 (finger 1 1))
  (define R2 (finger 1 2))
  (define R3 (finger 1 3))
  (define R4 (finger 1 4)))


;; ------------------------------------------------------------------------

;; A [Fingeringof X] is a (fingering Finger X)
(struct fingering [finger target] #:transparent)

;; A ChordFingering is a [Listof [Maybe [Fingeringof Interval]]]
;; For a given StringSpec, each fingering of an interval represents the
;; interval of the note above the string along with which finger should
;; play that note.

;; chord-fingering->chord : StringSpec ChordFingering -> Chord
(define (chord-fingering->chord strings chord-fingering)
  (for/list ([string (in-list strings)]
             [string-fingering (in-list chord-fingering)]
             #:when string-fingering)
    (note+ string (fingering-target string-fingering))))

|#

;; ------------------------------------------------------------------------

;; Which fingerings are impossible to play?

(provide chord-layout-stretch min-stretch-chord-layout)

;; TODO: What other qualities of chords should be considered? Minimum
;; stretch isn't always the best, and when there are multiple with the
;; same stretch, what should decide between them?

;; chord-layout-stretch : ChordLayout -> Nat
;; For now, only works for guitar-like instruments.
;; Also, it treats fret distances the same whether they're down low where
;; the frets are in reality farther apart, or up high where they are
;; closer together.
(define (chord-layout-stretch chord-layout)
  (define ns
    (for/list ([ivl (in-list chord-layout)]
               #:when ivl)
      (ivl-midi∆ ivl)))
  (define fingered-ns (filter positive? ns))
  (cond [(empty? fingered-ns) 0]
        [else
         (- (apply max fingered-ns)
            (apply min fingered-ns))]))

;; chord-layout-stretch<=? : Nat -> [ChordLayout -> Bool]
(define ((chord-layout-stretch<=? n) chord-layout)
  (<= (chord-layout-stretch chord-layout) n))

;; min-stretch-chord-layout : StringSpec Chord -> ChordLayout
(define (min-stretch-chord-layout strings chord)
  (argmin chord-layout-stretch
          (generate-chord-layouts strings chord)))

(module+ test
  (check-equal? (min-stretch-chord-layout guitar-strings
                                          (chord E2 major-triad))
                guitar-standard-E)

  (check-equal? (min-stretch-chord-layout guitar-strings
                                          (chord A2 major-triad))
                guitar-standard-A)

  (check-equal? (min-stretch-chord-layout guitar-strings
                                          (chord G2 major-triad))
                guitar-standard-G)

  ;; TODO: what should these tests be? Min-stretch isn't exactly what we
  ;; want here.
  #;
  (check-equal? (??? guitar-strings
                     (chord D3 major-triad))
                guitar-standard-D)
  #;
  (check-equal? (??? guitar-strings
                     (chord C3 major-triad))
                guitar-standard-C)
  #;
  (check-equal? (??? guitar-strings
                     (chord F3 major-triad))
                guitar-standard-F)
  )

;; ------------------------------------------------------------------------

