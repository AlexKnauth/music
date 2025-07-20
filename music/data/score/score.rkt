#lang agile

(require "../note/note.rkt"
         "../note/note-held.rkt"
         "../time/position.rkt"
         "../time/duration.rkt"
         "../time/time-period.rkt"
         "metadata.rkt"
         "clef.rkt"
         "key-signature.rkt"
         "../time/time-signature.rkt"
         "../time/tempo.rkt")
(module+ example
  (provide (all-defined-out))
  (require (submod "../note/note.rkt" example)
           (submod "../note/note-held.rkt" example)))

;; ------------------------------------------------------------------------

(provide score
         part part-name
         part-sorted-elements
         harmony-element harmony-element? harmony-element-chord-layout
         lyric lyric? lyric-number lyric-syllabic lyric-text
         here)

;; A Score is a
;; (score [Maybe MetaData] Key Duration [Listof Part])
(struct score [metadata parts] #:transparent)

;; A Part is a (part String SortedMusElements)
(define (part-guard name elements s-type)
  (unless (string? name)
    (error 'part "expected a string, given ~v" name))
  (unless (list? elements)
    (error 'part "expected a list, given ~v" elements))
  (unless (andmap timed? elements)
    (error 'part "expected a list of [Timed MusElement], given ~v" elements))
  (values name (sorted/time-period elements)))

(struct part [name sorted-elements] #:transparent
  #:guard part-guard)

;; A SortedMusElements is a [Listof MusElementThere]
;; Where they are sorted from earliest position to latest position.

;; A MusElementThere is a [Timed MusElement]
;; A MusElement is one of:
;;  - Key
;;  - TimeSignature
;;  - Tempo
;;  - Note
;;  - HarmonyElement
;;  - Lyric

;; A HarmonyElement is a (harmony-element ChordSymbol [Maybe ChordLayout])
(struct harmony-element [chord-symbol chord-layout] #:transparent)

;; A Lyric is a (lyric String Syllabic String)
(struct lyric [number syllabic text] #:transparent)

;; A Syllabic is one of:
;;  - 'begin
;;  - 'end
;;  - 'middle
;;  - 'single

;; ------------------------------------------------------------------------

(provide score-add-part)

(define (score-add-part s p)
  (match s
    [(score metadata parts)
     (score metadata (append parts (list p)))]))

;; ------------------------------------------------------------------------

(module+ example
  (define SIMPLE-EXAMPLE
    (score
     #false
     (list
      (part "Music"
        (sorted/time-period
         (here (position 0 beat-one) TREBLE-CLEF)
         (here (position 0 beat-one) (key 0))
         (here (position 0 beat-one) (time-sig/nd 4 duration-quarter))
         (here (position 0 beat-one) (tempo 100 duration-quarter))
         (here (position 0 beat-two) C4♩)
         (here (position 0 beat-three) D4♩)
         (here (position 0 beat-four) E4♩ G4♩)
         (here (position 1 beat-one) F4♩ A4♩)
         (here (position 1 beat-two) E4♪ B4♩)
         (here (position 1 beat-two/and) D4♪)
         (here (position 1 beat-three) E4𝅗𝅥 C5𝅗𝅥))))))

  (define CHANGING-TIME-SIG
    (score
     #false
     (list
      (part "Music"
        (sorted/time-period
         (here (position 0 beat-one) TREBLE-CLEF)
         (here (position 0 beat-one) (key 0))
         (here (position 0 beat-one) (time-sig/nd 1 duration-quarter))
         (here (position 0 beat-one) (tempo 100 duration-quarter))
         (here (position 0 beat-one) C4♩)
         (here (position 1 beat-one) (time-sig/nd 2 duration-quarter))
         (here (position 1 beat-one) D4♩)
         (here (position 2 beat-two) E4𝅗𝅥)
         (here (position 3 beat-one) (time-sig/nd 3 duration-quarter)))))))
  )

;; ------------------------------------------------------------------------

