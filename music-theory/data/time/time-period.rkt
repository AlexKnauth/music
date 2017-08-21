#lang agile

(require racket/generic
         "duration.rkt"
         "position.rkt")

;; ------------------------------------------------------------------------

(provide time-period time-period?
         time-period-start
         time-period-duration
         time-period-end
         time-period-contains-pos?)

;; A TimePeriod is a (time-period Position Duration)
(struct time-period [start duration] #:transparent
  #:methods gen:has-position
  [(define (get-position tp)
     (time-period-start tp))
   (define (set-position tp pos)
     (match tp
       [(time-period _ duration)
        (time-period pos duration)]))])

;; time-period-end : TimePeriod -> Position
(define (time-period-end tp)
  (position+ (time-period-start tp) (time-period-duration tp)))

;; time-period-contains-pos? : TimePeriod Position -> Bool
;; Currently does not account for measure length
(define (time-period-contains-pos? tp pos)
  (and (position<=? (time-period-start tp) pos)
       (position<? pos (time-period-end tp))))

;; ------------------------------------------------------------------------

(provide timed timed?
         timed-period
         timed-value
         timed-map)

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

;; timed-map : [Timed X] [X -> Y] -> [Timed Y]
(define (timed-map tx f)
  (match tx
    [(timed period x)
     (timed period (f x))]))

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

