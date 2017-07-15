#lang agile

(require "note.rkt"
         "note-held.rkt")

;; ------------------------------------------------------------------------

(provide note-there note-there-duration)

;; A NoteThere is a (note-there Position NoteHeld)
(struct note-there [start-position note-held] #:transparent)

;; note-there-duration : NoteThere -> Duration
(define (note-there-duration nt)
  (note-held-duration (note-there-note-held nt)))

;; ------------------------------------------------------------------------

(provide position position-measure-number
         position=? position<?
         position+ position∆)

;; A Position is a (position Nat Duration)
(struct position [measure-number position-in-measure] #:transparent)

;; position=? : Position Position -> Bool
(define (position=? a b)
  (and (= (position-measure-number a) (position-measure-number b))
       (duration=? (position-position-in-measure a)
                   (position-position-in-measure b))))

;; position<? : Position Position -> Bool
(define (position<? a b)
  (match* [a b]
    [[(position am ap) (position bm bp)]
     (or (< am bm)
         (and (= am bm)
              (duration<? ap bp)))]))

;; position+ : Position Duration -> Position
(define (position+ a bd)
  (match a
    [(position am ad)
     (position am (duration+ ad bd))]))

;; position∆ : Position Position -> Duration
(define (position∆ a b)
  (match* [a b]
    [[(position am ap) (position bm bp)]
     #:when (= am bm)
     (duration∆ ap bp)]))

;; ------------------------------------------------------------------------

;; Durations interpreted as a position within a measure

(provide beat-one
         beat-one/e
         beat-one/and
         beat-one/a
         beat-two
         beat-two/e
         beat-two/and
         beat-two/a
         beat-three
         beat-three/e
         beat-three/and
         beat-three/a
         beat-four
         beat-four/e
         beat-four/and
         beat-four/a)

(define beat-one (duration 0 1))
(define beat-two (duration 1 1))
(define beat-three (duration 2 1))
(define beat-four (duration 3 1))

(define beat-one/and (duration+ beat-one duration-eighth))
(define beat-two/and (duration+ beat-two duration-eighth))
(define beat-three/and (duration+ beat-three duration-eighth))
(define beat-four/and (duration+ beat-four duration-eighth))

(define beat-one/e (duration+ beat-one duration-sixteenth))
(define beat-two/e (duration+ beat-two duration-sixteenth))
(define beat-three/e (duration+ beat-three duration-sixteenth))
(define beat-four/e (duration+ beat-four duration-sixteenth))

(define beat-one/a (duration+ beat-one/and duration-sixteenth))
(define beat-two/a (duration+ beat-two/and duration-sixteenth))
(define beat-three/a (duration+ beat-three/and duration-sixteenth))
(define beat-four/a (duration+ beat-four/and duration-sixteenth))

;; ------------------------------------------------------------------------

