#lang agile

(provide note-there
         note-there?
         note-there-duration)

(require "note.rkt"
         "note-held.rkt"
         "../time/position.rkt"
         "../time/time-period.rkt")

;; The data representation for notes does not include ties. Instead, two tied
;; notes are represented by a single note whose duration happens to cross over
;; a measure boundary.

;; A NoteThere is a [Timed Note]

;; note-there : Position NoteHeld -> NoteThere
(define (note-there pos nh)
  (timed/pos pos nh))

;; note-there? : Any -> Bool
(define (note-there? v)
  (and (timed? v)
       (note? (timed-value v))))

;; note-there-duration : NoteThere -> Duration
(define (note-there-duration nt)
  (timed-duration nt))

