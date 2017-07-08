#lang agile

(require "note.rkt" "note-there.rkt")

(module+ example
  (provide (all-defined-out))
  (require (submod "note.rkt" example)))

;; ------------------------------------------------------------------------

(provide score
         part part-name
         measure measure-sorted-notes
         notes-there notes-there-notes)

;; A Score is a (score Tempo Duration [Listof Part])
(struct score [tempo measure-length parts] #:transparent)

;; A Tempo is a (tempo PosNum Duration)
(struct tempo [beats-per-minute beat-length] #:transparent)

;; A Part is a (part String [Listof Measure])
(struct part [name measures] #:transparent)

;; A Measure is a (measure SortedNotes)
(struct measure [sorted-notes] #:transparent)

;; A SortedNotes is a [Listof NotesThere]
;; Where they are sorted from earliest position to latest position,
;; and there are no duplicate positions.

;; sorted-notes : NotesThere ... -> SortedNotes
(define (sorted-notes . notess)
  (sort notess position<? #:key notes-there-position))

;; A NotesThere is a (notes-there Position [Listof NoteThere])
;; Where every note's start-position is the same as position
(struct notes-there [position notes] #:transparent)

;; notes-here : Position [Listof [List Note Duration]] -> NotesThere
(define (notes-here position notes/durations)
  (notes-there position
    (for/list ([note/duration (in-list notes/durations)])
      (match-define (list note duration) note/duration)
      (note-there position duration note))))

;; ------------------------------------------------------------------------

(module+ example
  (define SIMPLE-EXAMPLE
    (score
     (tempo 100 duration-quarter)
     duration-whole
     (list
      (part "Music"
        (list
         (measure
          (sorted-notes
           (notes-here (position 0 beat-two)
             (list (list C4 duration-quarter)))
           (notes-here (position 0 beat-three)
             (list (list D4 duration-quarter)))
           (notes-here (position 0 beat-four)
             (list (list E4 duration-quarter)
                   (list G4 duration-quarter)))))
         (measure
          (sorted-notes
           (notes-here (position 1 beat-one)
             (list (list F4 duration-quarter)
                   (list A4 duration-quarter)))
           (notes-here (position 1 beat-two)
             (list (list E4 duration-eighth)
                   (list B4 duration-quarter)))
           (notes-here (position 1 beat-two/and)
             (list (list D4 duration-eighth)))
           (notes-here (position 1 beat-three)
             (list (list E4 duration-half)
                   (list C5 duration-half)))))))))))

;; ------------------------------------------------------------------------

