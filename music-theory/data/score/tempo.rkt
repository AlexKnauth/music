#lang agile

;; ------------------------------------------------------------------------

(provide tempo tempo-beat-length)

;; A Tempo is a (tempo PosNum Duration)
(struct tempo [beats-per-minute beat-length] #:transparent)

;; ------------------------------------------------------------------------

