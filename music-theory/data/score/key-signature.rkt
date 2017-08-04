#lang agile

;; ------------------------------------------------------------------------

(provide key key-fifths)

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

;; ------------------------------------------------------------------------

