#lang agile

(provide measure
         measure?
         measure-time-sig
         measure-elements
         group-measures)

(require "position.rkt"
         "time-period.rkt"
         "time-signature.rkt")

;; ------------------------------------------------------------------------

;; A Measure is a (measure TimeSig SortedNotes)
(struct measure [time-sig elements] #:transparent)

;; ------------------------------------------------------------------------

;; group-measures : SortedNotes -> [Listof Measure]
(define (group-measures elems)
  (define init-st
    (state 0 (find-time-sig elems #f)))

  (group-measures/st init-st elems))

;; ------------------------------------------------------------------------

;; A State is a (state Nat TimeSig)
(struct state
  [n time-sig]
  #:transparent)

;; st+meas : State -> State
(define (st+meas s)
  (match s
    [(state n ts)
     (state (add1 n) ts)]))

;; st/find-time-sig : State SortedNotes -> State
(define (st/find-time-sig s elems)
  (match s
    [(state n ts)
     (state n (find-time-sig elems ts))]))

;; find-time-sig : SortedNotes X -> (U TimeSig X)
(define (find-time-sig elems default)
  (let ([ts-here (findf time-sig-there? elems)])
    (if ts-here (timed-value ts-here) default)))

;; ------------------------------------------------------------------------

;; group-measures/st : State SortedNotes -> [Listof Measure]
;; ASSUME all the elements in elems are at measure n or after
(define (group-measures/st st elems)
  (cond
    [(empty? elems) '()]
    [else
     (define n (state-n st))
     (define (measure-n? e)
       (= n (position-measure-number e)))

     (define-values [nth-measure rest]
       (partition measure-n? elems))

     (define st* (st/find-time-sig st nth-measure))
     (define ts* (state-time-sig st*))

     (define (roll-over e)
       (roll-over-measure e (time-sig-measure-length ts*)))

     (define-values [nth-measure* rolled-over]
       (partition measure-n? (map roll-over nth-measure)))

     (cons
      (measure ts* nth-measure*)
      (group-measures/st (st+meas st*) (append rolled-over rest)))]))

;; ------------------------------------------------------------------------

