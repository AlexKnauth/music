#lang agile

(require "position.rkt"
         "duration.rkt")

;; ------------------------------------------------------------------------

(provide tempo? tempo tempo-beat-length
         #;tempo-there?)

;; A Tempo is a (tempo PosNum Duration)
(struct tempo [beats-per-minute beat-length] #:transparent)

#|
;; tempo-there? : Any -> Bool
(define (tempo-there? v)
  (and (with-pos? v) (tempo? (with-pos-thing v))))
|#

;; ------------------------------------------------------------------------

