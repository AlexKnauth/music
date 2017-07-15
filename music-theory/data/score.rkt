#lang agile

(require "note.rkt" "note-held.rkt" "note-there.rkt")

(module+ example
  (provide (all-defined-out))
  (require (submod "note.rkt" example)
           (submod "note-held.rkt" example)))

;; ------------------------------------------------------------------------

(provide score
         key key-fifths
         tempo tempo-beat-length
         part part-name
         part-sorted-notes sorted-notes
         notes-there notes-there-position notes-there-notes
         notes-there-measure-number
         notes-here)

;; A Score is a (score Key Tempo Duration [Listof Part])
(struct score [key tempo measure-length parts] #:transparent)

;; A Key is a (key Int)
;; C = (key 0)
;; G = (key 1)
;; D = (key 2)
;; A = (key 3)
;; etc.
;; F = (key -1)
;; B♭ = (key -2)
;; E♭ = (key -3)
;; etc.
(struct key [fifths] #:transparent)

;; A Tempo is a (tempo PosNum Duration)
(struct tempo [beats-per-minute beat-length] #:transparent)

;; A Part is a (part String SortedNotes)
(struct part [name sorted-notes] #:transparent)

;; A SortedNotes is a [Listof NotesThere]
;; Where they are sorted from earliest position to latest position,
;; and there are no duplicate positions.

;; sorted-notes : NotesThere ... -> SortedNotes
(define (sorted-notes . notess)
  (sort notess position<? #:key notes-there-position))

;; A NotesThere is a (notes-there Position [Listof NoteThere])
;; Where every note's start-position is the same as position
(struct notes-there [position notes] #:transparent)

;; notes-there-measure-number : NotesThere -> Nat
(define (notes-there-measure-number nst)
  (position-measure-number (notes-there-position nst)))

;; notes-here : Position NoteHeld ... -> NotesThere
(define (notes-here position . notes-held)
  (notes-there position
    (for/list ([note-held (in-list notes-held)])
      (note-there position note-held))))

;; ------------------------------------------------------------------------

(module+ example
  (define SIMPLE-EXAMPLE
    (score
     (key 0)
     (tempo 100 duration-quarter)
     duration-whole
     (list
      (part "Music"
        (sorted-notes
         (notes-here (position 0 beat-two) C4♩)
         (notes-here (position 0 beat-three) D4♩)
         (notes-here (position 0 beat-four) E4♩ G4♩)
         (notes-here (position 1 beat-one) F4♩ A4♩)
         (notes-here (position 1 beat-two) E4♪ B4♩)
         (notes-here (position 1 beat-two/and) D4♪)
         (notes-here (position 1 beat-three) E4𝅗𝅥 C5𝅗𝅥)))))))

;; ------------------------------------------------------------------------

