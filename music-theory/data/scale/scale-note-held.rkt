#lang agile

(require "scale-note.rkt"
         "../note-held.rkt")
(module+ example
  (provide (all-defined-out))
  (require (submod "scale-note.rkt" example)
           "../../util/define-product-combinations.rkt"))

;; ------------------------------------------------------------------------

(provide scale-note-held
         scale-note-held?
         scale-note-held->note-held
         note-held->scale-note-held)

;; A ScaleNoteHeld is a (scale-note-held ScaleNote Duration)
(struct scale-note-held [note duration] #:transparent)

;; scale-note-held->note-held : ScaleNoteHeld -> NoteHeld
;; Should be called within `with-scale`
(define (scale-note-held->note-held snh)
  (match snh
    [(scale-note-held note duration)
     (note-held (scale-note->note note) duration)]))

;; note-held->scale-note-held : NoteHeld -> ScaleNoteHeld
;; Should be called within `with-scale`
(define (note-held->scale-note-held snh)
  (match snh
    [(note-held note duration)
     (scale-note-held (note->scale-note note) duration)]))

;; ------------------------------------------------------------------------

(module+ example
  (define-product-combinations scale-note-held
    [[s0:0 s0:0] [s0:1 s0:1] [s0:2 s0:2] [s0:3 s0:3] [s0:4 s0:4]
     [s1:0 s1:0] [s1:1 s1:1] [s1:2 s1:2] [s1:3 s1:3] [s1:4 s1:4]
     [s2:0 s2:0] [s2:1 s2:1] [s2:2 s2:2] [s2:3 s2:3] [s2:4 s2:4]
     [s3:0 s3:0] [s3:1 s3:1] [s3:2 s3:2] [s3:3 s3:3] [s3:4 s3:4]
     [s4:0 s4:0] [s4:1 s4:1] [s4:2 s4:2] [s4:3 s4:3] [s4:4 s4:4]
     [s5:0 s5:0] [s5:1 s5:1] [s5:2 s5:2] [s5:3 s5:3] [s5:4 s5:4]
     [s6:0 s6:0] [s6:1 s6:1] [s6:2 s6:2] [s6:3 s6:3] [s6:4 s6:4]

     [s3#:3 s3#:3]

     [s6♭:2 s6♭:2]
     ]
    [[𝅝 duration-whole]     [𝅝. duration-dotted-whole]
     [𝅗𝅥 duration-half]      [𝅗𝅥. duration-dotted-half]
     [♩ duration-quarter]   [♩. duration-dotted-quarter]
     [♪ duration-eighth]   [♪. duration-dotted-eighth]
     [𝅘𝅥𝅯 duration-sixteenth]])
  )

;; ------------------------------------------------------------------------

