#lang agile

(require "duration.rkt"
         "time-period.rkt")

;; ------------------------------------------------------------------------

(provide tempo? tempo)

;; A Tempo is a (tempo PosNum Duration)
(struct tempo [beats-per-minute beat-length] #:transparent)

;; ------------------------------------------------------------------------

(provide tempo-there?)

;; A TempoThere is a [Timed Tempo]

;; tempo-there? : Any -> Bool
(define (tempo-there? v)
  (and (timed? v) (tempo? (timed-value v))))

;; ------------------------------------------------------------------------

