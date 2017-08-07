#lang agile

(require "../time/position.rkt"
         "../time/time-period.rkt")

;; ------------------------------------------------------------------------

(provide key? key key-fifths
         key-there?)

;; A Key is a (key Int)
;; C = (key 0)
;; G = (key 1)
;; D = (key 2)
;; A = (key 3)
;; etc.
;; F = (key -1)
;; B♭ = (key -2)
;; E♭ = (key -3)
;; etc.
(struct key [fifths] #:transparent)

;; key-there? : Any -> Bool
(define (key-there? v)
  (and (timed? v) (key? (timed-value v))))

;; ------------------------------------------------------------------------

