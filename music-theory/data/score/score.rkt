#lang agile

(require "../note.rkt"
         "../note-held.rkt"
         "../position.rkt"
         "metadata.rkt"
         "key-signature.rkt"
         "tempo.rkt")
(module+ example
  (provide (all-defined-out))
  (require (submod "../note.rkt" example)
           (submod "../note-held.rkt" example)))

;; ------------------------------------------------------------------------

(provide score score-key score-measure-length
         part part-name
         part-sorted-elements sorted-elements
         harmony-element harmony-element-chord-layout
         here)

;; A Score is a
;; (score [Maybe MetaData] Key Tempo Duration [Listof Part])
(struct score [metadata key tempo measure-length parts] #:transparent)

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
    [(score work key tempo measure-length parts)
     (score work key tempo measure-length (append parts (list p)))]))

;; ------------------------------------------------------------------------

(module+ example
  (define SIMPLE-EXAMPLE
    (score
     #false
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

