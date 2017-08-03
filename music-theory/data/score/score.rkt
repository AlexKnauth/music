#lang agile

(require "../note.rkt"
         "../note-held.rkt"
         "../position.rkt")
(module+ example
  (provide (all-defined-out))
  (require (submod "../note.rkt" example)
           (submod "../note-held.rkt" example)))

;; ------------------------------------------------------------------------

(provide score score-key score-measure-length
         key key-fifths
         tempo tempo-beat-length
         part part-name
         part-sorted-elements sorted-elements
         harmony-element harmony-element-chord-layout
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

;; A Part is a (part String SortedMusElements)
(struct part [name sorted-elements] #:transparent)

;; A SortedElements is a [Listof MusElementThere]
;; Where they are sorted from earliest position to latest position.

;; sorted-elements : [Treeof NoteThere] ... -> SortedMusElements
(define sorted-elements sorted/position)

;; A MusElementThere is a [WithPos MusElement]
;; A MusElement is one of:
;;  - Note
;;  - HarmonyElement

;; A HarmonyElement is a (harmony-element ChordSymbol [Maybe ChordLayout])
(struct harmony-element [chord-symbol chord-layout] #:transparent)

;; ------------------------------------------------------------------------

(provide score-add-part)

(define (score-add-part s p)
  (match s
    [(score key tempo measure-length parts)
     (score key tempo measure-length (append parts (list p)))]))

;; ------------------------------------------------------------------------

(module+ example
  (define SIMPLE-EXAMPLE
    (score
     (key 0)
     (tempo 100 duration-quarter)
     duration-whole
     (list
      (part "Music"
        (sorted-elements
         (here (position 0 beat-two) C4♩)
         (here (position 0 beat-three) D4♩)
         (here (position 0 beat-four) E4♩ G4♩)
         (here (position 1 beat-one) F4♩ A4♩)
         (here (position 1 beat-two) E4♪ B4♩)
         (here (position 1 beat-two/and) D4♪)
         (here (position 1 beat-three) E4𝅗𝅥 C5𝅗𝅥)))))))

;; ------------------------------------------------------------------------

