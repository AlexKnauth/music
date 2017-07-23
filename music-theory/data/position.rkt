#lang agile

(require racket/generic
         "note-held.rkt")

;; ------------------------------------------------------------------------

(provide get-position
         set-position
         position-measure-number
         position-in-measure
         sorted/position
         here)

(define-generics has-position
  (get-position has-position)
  (set-position has-position new-pos))

;; position-measure-number : HasPosition -> Nat
(define (position-measure-number hp)
  (position-measure (get-position hp)))

;; position-in-measure : HasPosition -> Duration
(define (position-in-measure hp)
  (position-position-in-measure (get-position hp)))

;; sorted/position : [Treeof X] ... -> [Listof X]
;; where X <: HasPosition
(define (sorted/position . xs)
  (sort (flatten xs) position<? #:key get-position))

;; here : Position X ... -> [Listof [WithPos X]]
(define (here position . xs)
  (for/list ([x (in-list xs)])
    (with-pos position x)))

;; ------------------------------------------------------------------------

(provide with-pos with-pos-thing
         with-pos-map
         note-there-duration)

;; A NoteThere is a [WithPos NoteHeld]

;; A [WithPos X] is a (with-pos Position X)
(struct with-pos [position thing] #:transparent
  #:methods gen:has-position
  [(define (get-position this)
     (with-pos-position this))
   (define (set-position this new)
     (match this
       [(with-pos _ thing)
        (with-pos new thing)]))])

;; with-pos-map : [WithPos X] [X -> Y] -> [WithPos Y]
(define (with-pos-map wp f)
  (match wp
    [(with-pos p x)
     (with-pos p (f x))]))

(define (note-there-duration nt)
  (note-held-duration (with-pos-thing nt)))

;; ------------------------------------------------------------------------

(provide position
         position=? position<?
         position+ position∆
         position-measure+)

;; A Position is a (position Nat Duration)
(struct position [measure position-in-measure] #:transparent
  #:methods gen:has-position
  [(define (get-position this) this)
   (define (set-position this new) new)])

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

;; position-measure+ : Position Int -> Position
(define (position-measure+ a bm)
  (match a
    [(position am ad)
     (position (+ am bm) ad)]))

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

