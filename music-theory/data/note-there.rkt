#lang agile

(require "note.rkt")

;; ------------------------------------------------------------------------

(provide note-there note-there-duration)

;; A NoteThere is a (note-there Position Duration Note)
(struct note-there [start-position duration note] #:transparent)

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

(provide duration
         duration-common-divisions
         duration-n/divisions
         duration=?
         duration<?
         duration+
         duration-divide
         duration-fraction
         duration-zero
         duration-quarter
         duration-eighth
         duration-sixteenth
         duration-half
         duration-whole
         beat-one
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

;; A Duration is a (duration Nat PosInt)
(struct duration [n divisions] #:transparent)

;; duration=? : Duration Duration -> Bool
(define (duration=? a b)
  (match* [a b]
    [[(duration an ad) (duration bn bd)]
     (= (/ an ad) (/ bn bd))]))

;; duration<? : Duration Duration -> Bool
(define (duration<? a b)
  (match* [a b]
    [[(duration an ad) (duration bn bd)]
     (< (/ an ad) (/ bn bd))]))

;; duration-common-divisions : Duration ... -> PosInt
(define (duration-common-divisions . ds)
  (apply lcm (map duration-divisions ds)))

;; duration-n/divisions : Duration PosInt -> Nat
(define (duration-n/divisions d divisions)
  (match d
    [(duration dn dd)
     (unless (= (lcm dd divisions) divisions)
       (error 'duration-n/divisions "not divisible"))
     (* dn (/ divisions dd))]))

;; duration+ : Duration Duration -> Duration
(define (duration+ a b)
  (define d (duration-common-divisions a b))
  (duration
   (+ (duration-n/divisions a d)
      (duration-n/divisions b d))
   d))

;; duration∆ : Duration Duration -> Duration
(define (duration∆ a b)
  (define d (duration-common-divisions a b))
  (duration
   (- (duration-n/divisions b d)
      (duration-n/divisions a d))
   d))

;; duration-divide : Duration Duration -> (values Nat Duration)
(define (duration-divide a b)
  (define d (duration-common-divisions a b))
  (define an (duration-n/divisions a d))
  (define bn (duration-n/divisions b d))
  (define base (gcd an bn))
  (define n (/ an base))
  (values n (duration base d)))

;; duration-fraction : Duration Duration -> ExactRational
(define (duration-fraction a b)
  (match* [a b]
    [[(duration an ad) (duration bn bd)]
     (/ (/ an ad) (/ bn bd))]))

(define duration-zero (duration 0 1))
(define duration-quarter (duration 1 1))
(define duration-eighth (duration 1 2))
(define duration-sixteenth (duration 1 4))

(define duration-half (duration 2 1))
(define duration-whole (duration 4 1))

;; Interpreted as a position within a measure:
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

