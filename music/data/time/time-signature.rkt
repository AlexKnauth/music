#lang agile

(require "duration.rkt"
         "time-period.rkt")

;; ------------------------------------------------------------------------

(provide time-sig?
         time-sig/nd
         time-sig-measure-length
         time-sig->nd-values)

;; A TimeSignature is a (time-sig [Listof Duration])
(struct time-sig [beat-durations] #:transparent)

(define (time-sig/nd n d)
  (time-sig (make-list n d)))

(define (time-sig-measure-length ts)
  (match ts
    [(time-sig beat-durs)
     (for/fold ([ml duration-zero])
               ([dur (in-list beat-durs)])
       (duration+ ml dur))]))

(define (time-sig->nd-values ts)
  (define ml (time-sig-measure-length ts))
  (define-values [beats beat-type]
    (duration-divide ml (first (time-sig-beat-durations ts))))
  (values beats beat-type))

;; ------------------------------------------------------------------------

(provide time-sig-there?)

;; A TimeSigThere is a [Timed TimeSig]

;; time-sig-there? : Any -> Bool
(define (time-sig-there? v)
  (and (timed? v) (time-sig? (timed-value v))))

;; ------------------------------------------------------------------------

