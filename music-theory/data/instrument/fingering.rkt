#lang agile

(module+ example
  (provide (all-defined-out)))

;; ------------------------------------------------------------------------

;; TODO:
;;   Figure out where this is useful.
;;   Can simpler rules do just as well?

(provide fingering
         fingering-finger
         fingering-target)

;; A [Fingeringof X] is a (fingering [Maybe Finger] X)
(struct fingering [finger target] #:transparent)

;; A finger field of #false means that you don't need a finger for this,
;; usually because of an open string.

;; ------------------------------------------------------------------------

(provide finger finger?
         finger-hand finger-num
         finger=?)

;; A Finger is a (finger Hand FingerNum)
;; A FingerNum is a Int[0,4]
;; thumb   : 0
;; pointer : 1
;; middle  : 2
;; ring    : 3
;; pinkie  : 4
;; A Hand is an Int[0,1]
;; left  : 0
;; right : 1

(struct finger [hand num] #:transparent)

;; finger=? : Finger Finger -> Bool
(define (finger=? a b)
  (and (= (finger-hand a) (finger-hand b))
       (= (finger-num a) (finger-num b))))

(module+ example
  (define LH 0)
  (define RH 1)

  ;; left hand
  (define L0 (finger LH 0))
  (define L1 (finger LH 1))
  (define L2 (finger LH 2))
  (define L3 (finger LH 3))
  (define L4 (finger LH 4))

  ;; right hand
  (define R0 (finger RH 0))
  (define R1 (finger RH 1))
  (define R2 (finger RH 2))
  (define R3 (finger RH 3))
  (define R4 (finger RH 4)))

;; ------------------------------------------------------------------------

