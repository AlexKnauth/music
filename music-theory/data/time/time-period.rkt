#lang agile

(require racket/generic
         "duration.rkt"
         "position.rkt")
(module+ test
  (require rackunit))

;; The data representation for time periods does not include ties. Instead, two
;; tied notes are represented by a single note whose duration happens to cross
;; over a measure boundary.

;; ------------------------------------------------------------------------

(provide time-period time-period?
         time-period-start
         time-period-duration
         time-period-end
         time-period-contains-pos?
         time-period-overlap?
         time-period-split-over-measure)

;; A TimePeriod is a (time-period Position Duration)
(struct time-period [start duration] #:transparent
  #:methods gen:has-position
  [(define (get-position tp)
     (time-period-start tp))
   (define (set-position tp pos)
     (match tp
       [(time-period _ duration)
        (time-period pos duration)]))])

;; time-period=? : TimePeriod TimePeriod -> Boolean
(define (time-period=? a b)
  (match* [a b]
    [[(time-period ap ad) (time-period bp bd)]
     (and (position=? ap bp)
          (duration=? ad bd))]))

;; time-period-end : TimePeriod -> Position
(define (time-period-end tp)
  (position+ (time-period-start tp) (time-period-duration tp)))

;; time-period-contains-pos? : TimePeriod Position -> Bool
;; Currently does not account for measure length
(define (time-period-contains-pos? tp pos)
  (and (position<=? (time-period-start tp) pos)
       (position<? pos (time-period-end tp))))

;; time-period-overlap? : TimePeriod TimePeriod -> Bool
;; Currently does not account for measure length
;; (a-start < b-end) and (b-start < a-end)
(define (time-period-overlap? a b)
  (and (position<? (time-period-start a) (time-period-end b))
       (position<? (time-period-start b) (time-period-end a))))

;; time-period-split-over-measure : TimePeriod Duration -> [Listof TimePeriod]
;; ASSUME tp is in the correct measure
;; meas-dur is the duration of the measure that tp starts in
(define (time-period-split-over-measure tp meas-dur)
  (match tp
    [(time-period (position m p) d)
     (define end (duration+ p d))
     (cond
       [(duration<=? end meas-dur)  (list tp)]
       [(duration<? duration-zero meas-dur)
        (define fd (duration∆ p meas-dur))
        (list (time-period (position m p) fd)
              (time-period (position (add1 m) duration-zero)
                           (duration∆ fd d)))]
       [else
        (error 'time-period-split-over-measures
               "measures must have non-zero length")])]))

;; ------------------------------------------------------------------------

(provide timed timed?
         timed-period
         timed-value
         timed-duration
         timed-end
         timed-map
         timed-overlap?
         timed-split-over-measure/no-tie)

;; A [Timed X] is a (timed TimePeriod X)
(struct timed [period value] #:transparent
  #:methods gen:has-position
  [(define/generic gen-set-position set-position)
   (define (get-position t)
     (time-period-start (timed-period t)))
   (define (set-position t pos)
     (match t
       [(timed tp value)
        (timed (gen-set-position tp pos) value)]))])

;; timed-duration : [Timed X] -> Duration
(define (timed-duration tx)
  (time-period-duration (timed-period tx)))

;; timed-end : [Timed X] -> Position
(define (timed-end tx)
  (time-period-end (timed-period tx)))

;; timed-map : [Timed X] [X -> Y] -> [Timed Y]
(define (timed-map tx f)
  (match tx
    [(timed period x)
     (timed period (f x))]))

;; timed-overlap? : [Timed X] [Timed Y] -> Bool
(define (timed-overlap? a b)
  (time-period-overlap? (timed-period a) (timed-period b)))

;; timed-split-over-measures/no-tie : [Timed X] Duration -> [Listof [Timed X]]
;; ASSUME tx is in the correct measure
;; meas-dur is the duration of the measure that tx starts in
;; Typically the caller will handle the output to add ties its way.
(define (timed-split-over-measure/no-tie tx meas-dur)
  (match tx
    [(timed period x)
     (map
      (λ (p) (timed p x))
      (time-period-split-over-measure period meas-dur))]))

;; ------------------------------------------------------------------------

;; Grouping by time period

(provide group-by-time-period)

;; group-by-time-period : [Listof [Timed X]] -> [Listof [Timed [NEListof X]]]
(define (group-by-time-period txs)
  (define groups
    (group-by timed-period txs time-period=?))
  (for/list ([g (in-list groups)])
    (timed
     (timed-period (first g))
     (map timed-value g))))

;; ------------------------------------------------------------------------

;; Things that have duration, but no defined position

(provide lasting lasting?
         lasting-duration
         lasting-value
         timed/pos
         here)

;; A [Lasting X] is a (lasting Duration X)
(struct lasting [duration value] #:transparent)

(define (timed/pos pos v)
  (cond [(lasting? v)
         (timed (time-period pos (lasting-duration v)) (lasting-value v))]
        [else
         (timed (time-period pos duration-zero) v)]))

(define (here pos . xs)
  (for/list ([x (in-list xs)])
    (timed/pos pos x)))

;; ------------------------------------------------------------------------

(module+ test
  (check-true (time-period-overlap?
               (time-period (position 0 beat-two) duration-quarter)
               (time-period (position 0 beat-one) duration-dotted-quarter)))
  (check-false (time-period-overlap?
                (time-period (position 5 beat-two) duration-quarter)
                (time-period (position 5 beat-one) duration-quarter)))
  (check-true (time-period-overlap?
               (time-period (position 2 beat-two) duration-quarter)
               (time-period (position 2 beat-one) duration-whole)))
  (check-false (time-period-overlap?
                (time-period (position 6 beat-one) duration-quarter)
                (time-period (position 6 beat-two) duration-whole)))
  (check-true (time-period-overlap?
               (time-period (position 3 beat-one/and) duration-quarter)
               (time-period (position 3 beat-two) duration-whole)))
  )

;; ------------------------------------------------------------------------

