#lang agile

(require "../data/note.rkt"
         "../data/scale/scale-note.rkt"
         "../data/note-held.rkt"
         "../data/note-there.rkt"
         "../data/score/score.rkt"
         (submod "../data/note.rkt" example)
         (submod "../data/note-held.rkt" example))
(module+ test
  (require racket/runtime-path
           "../notation/musicxml/musicxml-file.rkt"
           "../notation/musicxml/score.rkt"))

(define (transform/time sorted-notes f)
  (for/list ([n (in-list sorted-notes)])
    (match n
      [(note-there pos n)
       (note-there (f pos) n)])))

(define (transform/note sorted-notes f)
  (for/list ([n (in-list sorted-notes)])
    (match n
      [(note-there pos (note-held n d))
       (note-there pos (note-held (f n) d))])))

;; From BWV 988: Goldberg Variations, Variation 12, Canone alla Quarta

(define melody
  (sorted-notes
   (notes-here (position 0 beat-one/and)   G4𝅘𝅥𝅯)
   (notes-here (position 0 beat-one/a)     F#4𝅘𝅥𝅯)
   (notes-here (position 0 beat-two)       G4♪)
   (notes-here (position 0 beat-two/and)   A4𝅘𝅥𝅯)
   (notes-here (position 0 beat-two/a)     B4𝅘𝅥𝅯)
   (notes-here (position 0 beat-three)     C5𝅘𝅥𝅯)
   (notes-here (position 0 beat-three/e)   B4𝅘𝅥𝅯)
   (notes-here (position 0 beat-three/and) A4𝅘𝅥𝅯)
   (notes-here (position 0 beat-three/a)   G4𝅘𝅥𝅯)
   (notes-here (position 1 beat-one)       D5𝅘𝅥𝅯)
   (notes-here (position 1 beat-one/e)     A4𝅘𝅥𝅯)
   (notes-here (position 1 beat-one/and)   B4𝅘𝅥𝅯)
   (notes-here (position 1 beat-one/a)     C#5𝅘𝅥𝅯)
   (notes-here (position 1 beat-two)       D5𝅘𝅥𝅯)
   (notes-here (position 1 beat-two/e)     E5𝅘𝅥𝅯)
   (notes-here (position 1 beat-two/and)   F#5𝅘𝅥𝅯)
   (notes-here (position 1 beat-two/a)     G5𝅘𝅥𝅯)
   (notes-here (position 1 beat-three)     A5♪)
   (notes-here (position 2 beat-one/and)   G5𝅘𝅥𝅯)
   (notes-here (position 2 beat-one/a)     F#5𝅘𝅥𝅯)
   (notes-here (position 2 beat-two)       E5𝅘𝅥𝅯)
   (notes-here (position 2 beat-two/e)     D5𝅘𝅥𝅯)
   (notes-here (position 2 beat-two/and)   C#5𝅘𝅥𝅯)
   (notes-here (position 2 beat-two/a)     B4𝅘𝅥𝅯)
   (notes-here (position 2 beat-three)     A4𝅘𝅥𝅯)
   (notes-here (position 2 beat-three/e)   G4𝅘𝅥𝅯)
   (notes-here (position 2 beat-three/and) F#4𝅘𝅥𝅯)
   (notes-here (position 2 beat-three/a)   G4𝅘𝅥𝅯)
   (notes-here (position 3 beat-one)       G4♪)
   (notes-here (position 3 beat-one/and)   F#4𝅘𝅥𝅯)
   (notes-here (position 3 beat-one/a)     E4𝅘𝅥𝅯)
   (notes-here (position 3 beat-two)       D4♪)
   (notes-here (position 4 beat-one/e)     D5♪.) ;; TODO: tie
   (notes-here (position 4 beat-two)       D5𝅘𝅥𝅯)
   (notes-here (position 4 beat-two/e)     B4𝅘𝅥𝅯)
   (notes-here (position 4 beat-two/and)   A4𝅘𝅥𝅯)
   (notes-here (position 4 beat-two/a)     G4𝅘𝅥𝅯)
   (notes-here (position 4 beat-three)     F4𝅘𝅥𝅯)
   (notes-here (position 4 beat-three/e)   E4𝅘𝅥𝅯)
   (notes-here (position 4 beat-three/and) D4𝅘𝅥𝅯)
   (notes-here (position 4 beat-three/a)   F4𝅘𝅥𝅯)
   (notes-here (position 5 beat-one)       E4𝅘𝅥𝅯)
   (notes-here (position 5 beat-one/e)     G4𝅘𝅥𝅯)
   (notes-here (position 5 beat-one/and)   C5𝅘𝅥𝅯)
   (notes-here (position 5 beat-one/a)     D5𝅘𝅥𝅯)
   (notes-here (position 5 beat-two)       E5♪)
   (notes-here (position 5 beat-two/and)   A4♩)
   (notes-here (position 5 beat-three/and) A4♪) ; TODO: tie
   (notes-here (position 6 beat-one)       A4𝅘𝅥𝅯)
   (notes-here (position 6 beat-one/e)     D4𝅘𝅥𝅯)
   (notes-here (position 6 beat-one/and)   F#4𝅘𝅥𝅯)
   (notes-here (position 6 beat-one/a)     G4𝅘𝅥𝅯)
   (notes-here (position 6 beat-two)       A4𝅘𝅥𝅯)
   (notes-here (position 6 beat-two/e)     G4𝅘𝅥𝅯)
   (notes-here (position 6 beat-two/and)   F#4𝅘𝅥𝅯)
   (notes-here (position 6 beat-two/a)     E5𝅘𝅥𝅯)
   (notes-here (position 6 beat-three)     D5𝅘𝅥𝅯)
   (notes-here (position 6 beat-three/e)   C5𝅘𝅥𝅯)
   (notes-here (position 6 beat-three/and) B4𝅘𝅥𝅯)
   (notes-here (position 6 beat-three/a)   A4𝅘𝅥𝅯)
   (notes-here (position 7 beat-one)       G4♩)
   ))

(define bass
  (sorted-notes
   (notes-here (position 0 beat-one)       G3♩)
   (notes-here (position 0 beat-two)       G3♩)
   (notes-here (position 0 beat-three)     G3♩)
   (notes-here (position 1 beat-one)       F#3♩)
   (notes-here (position 1 beat-two)       F#3♩)
   (notes-here (position 1 beat-three)     F#3♩)
   (notes-here (position 2 beat-one)       E3♩)
   (notes-here (position 2 beat-two)       E3♩)
   (notes-here (position 2 beat-three)     E3♩)
   (notes-here (position 3 beat-one)       D3𝅘𝅥𝅯)
   (notes-here (position 3 beat-one/e)     D2𝅘𝅥𝅯)
   (notes-here (position 3 beat-one/and)   D3♪)
   (notes-here (position 3 beat-two/and)   C3𝅘𝅥𝅯)
   (notes-here (position 3 beat-two/a)     B2𝅘𝅥𝅯)
   (notes-here (position 3 beat-three)     A2♪)
   (notes-here (position 3 beat-three/and) C3♪)
   (notes-here (position 4 beat-one)       B2♩)
   (notes-here (position 4 beat-two)       B2♩)
   (notes-here (position 4 beat-three)     B2♩)
   (notes-here (position 5 beat-one)       C3♩)
   (notes-here (position 5 beat-two)       C3♩)
   (notes-here (position 5 beat-three)     C3♩)
   (notes-here (position 6 beat-one)       D3♩)
   (notes-here (position 6 beat-two)       D3♩)
   (notes-here (position 6 beat-three)     D3♩)
   (notes-here (position 7 beat-one)       G3♪.)
   (notes-here (position 7 beat-one/a)     B2𝅘𝅥𝅯)
   (notes-here (position 7 beat-two)       E3𝅘𝅥𝅯)
   (notes-here (position 7 beat-two/e)     D3𝅘𝅥𝅯)
   (notes-here (position 7 beat-two/and)   C3𝅘𝅥𝅯)
   (notes-here (position 7 beat-two/a)     E3𝅘𝅥𝅯)
   (notes-here (position 7 beat-three)     D3𝅘𝅥𝅯)
   (notes-here (position 7 beat-three/e)   C3𝅘𝅥𝅯)
   (notes-here (position 7 beat-three/and) B2𝅘𝅥𝅯)
   (notes-here (position 7 beat-three/a)   A2𝅘𝅥𝅯)
   (notes-here (position 8 beat-one)       G2♩)
   ))

(define melody-transformed
  (transform/note
   (transform/time
    melody
    (λ (pos)
      (position-measure+ pos 1)))
   (λ (note)
     (scale-note->note
      G4
      major
      (match (note->scale-note G4 major note)
        [(scale-note N i alteration)
         (scale-note N (+ (- i) -3) 0)])))))

(define Bach-Goldberg-Canone-alla-Quarta
  (score
   (key 1)
   (tempo 80 duration-quarter)
   (duration 3 1)
   (list
    (part "Melody" melody)
    (part "Melody-Transformed" melody-transformed)
    (part "Bass" bass))))

;; ------------------------------------------------------------------------

(module+ test
  (define-runtime-path Bach-Goldberg-Canone-alla-Quarta.xml
    "Bach-Goldberg-Canone-alla-Quarta.xml")

  (write-musicxml-file Bach-Goldberg-Canone-alla-Quarta.xml
                       (score->musicxml Bach-Goldberg-Canone-alla-Quarta)
                       #:exists 'replace)

  (open-musicxml-file/MuseScore-2 Bach-Goldberg-Canone-alla-Quarta.xml)
  )

;; ------------------------------------------------------------------------

