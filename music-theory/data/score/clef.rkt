#lang agile

(require "../time/time-period.rkt")

;; ------------------------------------------------------------------------

(provide clef clef?
         clef-type clef-type->sign+line
         TREBLE-CLEF
         BASS-CLEF
         ALTO-CLEF)

;; A ClefType is one of
;;  - (treble)
;;  - (bass)
;;  - (alto)
(struct treble [])
(struct bass [])
(struct alto [])

;; clef-type->sign+line : ClefType -> (values String Nat)
(define (clef-type->sign+line ct)
  (match ct
    [(treble) (values "G" 2)]
    [(bass) (values "F" 4)]
    [(alto) (values "C" 3)]))

;; A Clef is a (clef ClefType)
;; TODO: handle offset versions, such as the tenor clef
;; TODO: handle octave-shifted versions
(struct clef [type] #:transparent)

(define TREBLE-CLEF (clef (treble)))
(define BASS-CLEF (clef (bass)))
(define ALTO-CLEF (clef (alto)))

;; ------------------------------------------------------------------------

(provide clef-there?)

;; A ClefThere is a [Timed Clef]

;; clef-there? : Any -> Bool
(define (clef-there? v)
  (and (timed? v) (clef? (timed-value v))))

;; ------------------------------------------------------------------------

