#lang agile

(require "../note.rkt"
         "../note-held.rkt"
         "../position.rkt")
(module+ example
  (provide (all-defined-out))
  (require (submod "../note.rkt" example)
           (submod "../note-held.rkt" example)))

;; ------------------------------------------------------------------------

(provide score score-key
         key key-fifths
         tempo tempo-beat-length
         part part-name
         part-sorted-notes sorted-notes
         here)

;; A Score is a (score Key Tempo Duration [Listof Part])
(struct score [key tempo measure-length parts] #:transparent)

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

;; A Tempo is a (tempo PosNum Duration)
(struct tempo [beats-per-minute beat-length] #:transparent)

;; A Part is a (part String SortedNotes)
(struct part [name sorted-notes] #:transparent)

;; A SortedNotes is a [Listof NoteThere]
;; Where they are sorted from earliest position to latest position.

;; sorted-notes : [Treeof NoteThere] ... -> SortedNotes
(define sorted-notes sorted/position)

;; ------------------------------------------------------------------------

(module+ example
  (define SIMPLE-EXAMPLE
    (score
     (key 0)
     (tempo 100 duration-quarter)
     duration-whole
     (list
      (part "Music"
        (sorted-notes
         (here (position 0 beat-two) C4♩)
         (here (position 0 beat-three) D4♩)
         (here (position 0 beat-four) E4♩ G4♩)
         (here (position 1 beat-one) F4♩ A4♩)
         (here (position 1 beat-two) E4♪ B4♩)
         (here (position 1 beat-two/and) D4♪)
         (here (position 1 beat-three) E4𝅗𝅥 C5𝅗𝅥)))))))

;; ------------------------------------------------------------------------

