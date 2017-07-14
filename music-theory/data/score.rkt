#lang agile

(require "note.rkt" "note-there.rkt")

(module+ example
  (provide (all-defined-out))
  (require (submod "note.rkt" example)))

;; ------------------------------------------------------------------------

(provide score
         key key-fifths
         tempo tempo-beat-length
         part part-name
         measure measure* measure-sorted-notes
         notes-there notes-there-notes
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

;; A Part is a (part String [Listof Measure])
(struct part [name measures] #:transparent)

;; A Measure is a (measure SortedNotes)
(struct measure [sorted-notes] #:transparent)

(define (measure* . notess)
  (measure (apply sorted-notes notess)))

;; A SortedNotes is a [Listof NotesThere]
;; Where they are sorted from earliest position to latest position,
;; and there are no duplicate positions.

;; sorted-notes : NotesThere ... -> SortedNotes
(define (sorted-notes . notess)
  (sort notess position<? #:key notes-there-position))

;; A NotesThere is a (notes-there Position [Listof NoteThere])
;; Where every note's start-position is the same as position
(struct notes-there [position notes] #:transparent)

;; notes-here : Position [List Note Duration] ... -> NotesThere
(define (notes-here position . notes/durations)
  (notes-there position
    (for/list ([note/duration (in-list notes/durations)])
      (match-define (list note duration) note/duration)
      (note-there position duration note))))

;; ------------------------------------------------------------------------

(module+ example
  (define SIMPLE-EXAMPLE
    (score
     (key 0)
     (tempo 100 duration-quarter)
     duration-whole
     (list
      (part "Music"
        (list
         (measure*
          (notes-here (position 0 beat-two)
            (list C4 duration-quarter))
          (notes-here (position 0 beat-three)
            (list D4 duration-quarter))
          (notes-here (position 0 beat-four)
            (list E4 duration-quarter)
            (list G4 duration-quarter)))
         (measure*
          (notes-here (position 1 beat-one)
            (list F4 duration-quarter)
            (list A4 duration-quarter))
          (notes-here (position 1 beat-two)
            (list E4 duration-eighth)
            (list B4 duration-quarter))
          (notes-here (position 1 beat-two/and)
            (list D4 duration-eighth))
          (notes-here (position 1 beat-three)
            (list E4 duration-half)
            (list C5 duration-half)))))))))

;; ------------------------------------------------------------------------

