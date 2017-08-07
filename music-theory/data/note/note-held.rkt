#lang agile

(require "note.rkt"
         "../time/duration.rkt"
         "../time/time-period.rkt")
(module+ example
  (provide (all-defined-out))
  (require (submod "note.rkt" example)
           music-theory/util/define-product-combinations))

;; ------------------------------------------------------------------------

(provide note-held
         note-held?
         note-held-note
         note-held-duration)

;; A NoteHeld is a [Lasting Note]
(define (note-held note duration)
  (lasting duration note))

(define (note-held? v)
  (and (lasting? v) (note? (lasting-value v))))

(define (note-held-note nh) (lasting-value nh))
(define (note-held-duration nh) (lasting-duration nh))

(module+ example
  (define-product-combinations note-held
    [[C2 C2] [C3 C3] [C4 C4] [C5 C5]
     [D2 D2] [D3 D3] [D4 D4] [D5 D5]
     [E2 E2] [E3 E3] [E4 E4] [E5 E5]
     [F2 F2] [F3 F3] [F4 F4] [F5 F5]
     [G2 G2] [G3 G3] [G4 G4] [G5 G5]
     [A2 A2] [A3 A3] [A4 A4] [A5 A5]
     [B2 B2] [B3 B3] [B4 B4] [B5 B5]
     [F#3 F#3] [F#4 F#4] [F#5 F#5]
     [C#3 C#3] [C#4 C#4] [C#5 C#5]]
    [[𝅝 duration-whole]     [𝅝. duration-dotted-whole]
     [𝅗𝅥 duration-half]      [𝅗𝅥. duration-dotted-half]
     [♩ duration-quarter]   [♩. duration-dotted-quarter]
     [♪ duration-eighth]   [♪. duration-dotted-eighth]
     [𝅘𝅥𝅯 duration-sixteenth]])
  )

;; ------------------------------------------------------------------------

