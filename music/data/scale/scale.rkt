#lang agile

(require "../note/note.rkt")

;; ------------------------------------------------------------------------

(provide scale
         with-scale
         (rename-out [get-current-scale current-scale]))

;; A Scale is a (scale Note ScaleKind)
(struct scale [root kind] #:transparent)

;; current-scale : [Parameterof [Maybe Scale]]
(define current-scale (make-parameter #f))

;; call-with-scale : Scale [-> X] -> X
(define (call-with-scale s f)
  (parameterize ([current-scale s]) (f)))

(define-simple-macro (with-scale s:expr body:expr ...+)
  (call-with-scale s (λ () body ...)))

(define (get-current-scale)
  (define s (current-scale))
  (unless (scale? s)
    (error 'current-scale "must be called within `with-scale`"))
  s)

;; ------------------------------------------------------------------------

(provide scale-notes)

;; scale-notes : Scale -> [Listof Note]
(define (scale-notes s)
  (match-define (scale root kind) s)
  (for/list ([ivl (in-list kind)])
    (note+ root ivl)))

;; ------------------------------------------------------------------------

(provide major
         natural-minor
         harmonic-minor
         melodic-minor/ascending

         lydian
         mixolydian
         dorian
         phrygian
         locrian

         major-pentatonic
         relative-minor-pentatonic
         parallel-minor-pentatonic
         minor-blues)

;; A ScaleKind is a [Listof Interval]
(define major
  (list unison M2nd M3rd P4th P5th M6th M7th))
(define natural-minor
  (list unison M2nd m3rd P4th P5th m6th m7th))
(define harmonic-minor
  (list unison M2nd m3rd P4th P5th m6th M7th))
(define melodic-minor/ascending
  (list unison M2nd m3rd P4th P5th M6th M7th))

(define lydian
  (list unison M2nd M3rd A4th P5th M6th M7th))
(define mixolydian
  (list unison M2nd M3rd P4th P5th M6th m7th))
(define dorian
  (list unison M2nd m3rd P4th P5th M6th m7th))
(define phrygian
  (list unison m2nd m3rd P4th P5th m6th m7th))
(define locrian
  (list unison m2nd m3rd P4th d5th m6th m7th))

(define major-pentatonic
  (list unison M2nd M3rd P5th M6th))
(define relative-minor-pentatonic
  (list unison m3rd P4th P5th m7th))
(define parallel-minor-pentatonic
  (list unison M2nd m3rd P5th m6th))

(define minor-blues
  (list unison m3rd P4th A4th P5th m7th))

;; ------------------------------------------------------------------------

