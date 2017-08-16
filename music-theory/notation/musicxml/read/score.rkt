#lang agile

(require music-theory/util/txexpr
         "musicxml-file.rkt"
         "metadata.rkt"
         (prefix-in data/
           (combine-in
            music-theory/data/time/main
            music-theory/data/note/main
            music-theory/data/score/main)))
(module+ test
  (require rackunit
           music-theory/data/time/main
           (submod music-theory/data/note/note example)
           music-theory/example/Bach-Goldberg-Canone-alla-Quarta
           "../score.rkt"))

;; ------------------------------------------------------------------------

(provide musicxml->score)

;; musicxml->score : MXexpr -> Score
(define (musicxml->score mx)
  (match mx
    [(txexpr 'score-partwise (or '() '([version "3.0"]))
       (list
        (and metadata-elements (not (txexpr 'part-list _ _)
                                    (txexpr 'part _ _)))
        ...
        (and part-list (txexpr 'part-list _ _))
        (and parts (txexpr 'part _ _))
        ...))
     (data/score
      ; TODO: use metadata-elements
      (musicxml-elements->metadata metadata-elements)
      ; TODO: use part-list and parts
      (musicxml->parts part-list (musicxml-parts->hash parts)))]))

;; musicxml-parts->hash : [Listof MXexpr] -> [Hashof String MXexpr]
(define (musicxml-parts->hash parts)
  (for/hash ([p (in-list parts)])
    (values (attr-ref p 'id) p)))

;; musicxml->parts : MXexpr [Hashof String MXexpr] -> [Listof Part]
(define (musicxml->parts part-list parts-hsh)
  (match part-list
    [(txexpr 'part-list '() elements)
     (musicxml-elements->parts elements parts-hsh)]))

;; musicxml-elements->parts :
;; [Listof MXexpr] [Hahsof String MXexpr] -> [Listof Part]
(define (musicxml-elements->parts part-entries parts-hsh)
  (for/list ([part-entry (in-list part-entries)])
    (match part-entry
      [(txexpr 'score-part attrs
         (list (txexpr 'part-name '() (leaf/str name))
               (or (txexpr 'part-abbreviation _ _)
                   (txexpr 'score-instrument _ _)
                   (txexpr 'midi-device _ _)
                   (txexpr 'midi-instrument _ _))
               ...
               ))
       (define part (hash-ref parts-hsh (attr-ref part-entry 'id)))
       (musicxml->part name part)])))

;; musicxml->part : String MXexpr -> Part
(define (musicxml->part name part)
  (match part
    [(txexpr 'part _ (list (and measures (txexpr 'measure _ _)) ...))
     (data/part
      name
      (data/sorted/position
       (musicxml-measures->muselements measures 1 '())))]))

;; musicxml-measures->muselements :
;; [Listof MXexpr] Nat [Listof MusElement] -> [Listof MusElement]
(define (musicxml-measures->muselements ms div acc)
  (match ms
    ['() (reverse acc)]
    [(cons m ms)
     (musicxml-measure->muselements m ms div acc)]))

;; musicxml-measure->muselements :
;; MXexpr [Listof MXexpr] Nat [Listof MusElement] -> [Listof MusElement]
;; Order in the result list doesn't matter
(define (musicxml-measure->muselements m rst div acc)
  (match m
    [(txexpr 'measure attrs elements)
     (define mn (sub1 (string->number (attr-ref m 'number))))
     (define pos (data/position mn data/beat-one))
     (musicxml-elements->muselements
      elements
      rst
      pos
      div
      acc)]))

;; musicxml-elements->muselements :
;; [Listof MXexpr] [Listof MXexpr] Position Nat [Listof MusElement]
;; -> [Listof MusElement]
;; Order in the result list doesn't matter
(define (musicxml-elements->muselements mxs rst-measures pos div acc)
  (match mxs
    ['()
     (musicxml-measures->muselements rst-measures div acc)]
    [(cons fst rst)
     (match fst
       [(txexpr 'attributes '()
          (list (txexpr 'divisions '() (leaf/num div))
                other
                ...))
        (musicxml-elements->muselements
          (cons (txexpr 'attributes '() other) rst)
          rst-measures
          pos
          div
          acc)]
       [(txexpr 'attributes '()
          (list (and elements (not (txexpr 'divisions _ _))) ...))
        (musicxml-elements->muselements rst rst-measures pos div
          (for/fold ([acc acc])
                    ([elem (in-list elements)])
            (append (attributes-element->muselements elem pos)
                    acc)))]
       [(txexpr 'backup '()
          (list (txexpr 'duration '() (leaf/num dur))))
        (musicxml-elements->muselements rst rst-measures
          (data/position- pos (data/duration dur div))
          div
          acc)]
       [(txexpr 'forward '()
          (list (txexpr 'duration '() (leaf/num dur))))
        (musicxml-elements->muselements rst rst-measures
          (data/position+ pos (data/duration dur div))
          div
          acc)]
       [(txexpr 'harmony _ _)
        ('....)]
       [(txexpr 'direction _ _)
        (musicxml-elements->muselements rst rst-measures pos div
          (musicxml-direction->muselements fst pos acc))]
       [(txexpr 'note _ _)
        (musicxml-note->muselements fst rst rst-measures pos div acc)])]))

;; musicxml-direction->muselements :
;; MXexpr [Listof MusElement] -> [Listof MusElement]
(define (musicxml-direction->muselements mx pos acc)
  (match mx
    [(txexpr 'direction attrs
       (list
        (txexpr 'direction-type '()
          (list (txexpr 'metronome '()
                  (list (and beat-stuff (or (txexpr 'beat-unit _ _)
                                            (txexpr 'beat-unit-dot _ _)))
                        ...
                        (txexpr 'per-minute '() (leaf/num bpm))))))
        (txexpr 'sound _ _)))
     (cons
      (data/timed/pos
       pos
       (data/tempo
        bpm
        (match beat-stuff
          [(list (txexpr beat-unit '() (leaf/str "whole"))) data/duration-whole]
          [(list (txexpr beat-unit '() (leaf/str "half"))) data/duration-half]
          [(list (txexpr beat-unit '() (leaf/str "quarter"))) data/duration-quarter]
          [(list (txexpr beat-unit '() (leaf/str "eighth"))) data/duration-eighth]
          [(list (txexpr beat-unit '() (leaf/str "16th"))) data/duration-sixteenth]
          )))
       acc)]))

;; musicxml-note->muselements :
;; MXexpr [Listof MXexpr] [Listof MXexpr] Position Nat [Listof MusElement]
;; -> [Listof MusElement]
(define (musicxml-note->muselements note rst rst-measures pos div acc)
  (match note
    [(txexpr 'note attrs
       (list (txexpr 'rest '() _)
             (txexpr 'duration '() (leaf/num dur))
             (or (txexpr 'voice _ _)
                 (txexpr 'staff _ _)
                 (txexpr 'type _ _)
                 (txexpr 'dot _ _))
             ...))
     (musicxml-elements->muselements rst rst-measures
       (data/position+ pos (data/duration dur div))
       div
       acc)]
    [(txexpr 'note attrs
       (list (txexpr 'pitch _ _)
             (txexpr 'duration _ _)
             (not (txexpr 'rest _ _)
                  (txexpr 'chord _ _))
             ...))
     (define nt (musicxml->note-there note pos div))
     (musicxml-elements->muselements rst rst-measures
       (data/position+ pos (data/note-there-duration nt))
       div
       (cons nt acc))]
    [(txexpr 'note attrs
       (list (txexpr 'chord '() '())
             others
             ...))
     (define prev-time-period (data/timed-period (first acc)))
     (define prev-pos (data/time-period-start prev-time-period))
     (define nt (musicxml->note-there (txexpr 'note '() others) prev-pos div))
     (unless (equal? (data/timed-period nt) prev-time-period)
       (error 'chord "notes not same duration"))
     (musicxml-elements->muselements rst rst-measures
       pos
       div
       (cons nt acc))]))

;; musicxml->note-there : MXexpr Position Nat -> NoteThere
(define (musicxml->note-there note pos div)
  (match note
    [(txexpr 'note attrs
       (list (and pitch (txexpr 'pitch _ _))
             (txexpr 'duration '() (leaf/num dur))
             (or (txexpr 'type _ _)
                 (txexpr 'dot _ _))
             ...))
     (data/timed (data/time-period pos (data/duration dur div))
                 (musicxml-pitch->note pitch))]))

;; musicxml-pitch->note : MXexpr -> Note
(define (musicxml-pitch->note pitch)
  (match pitch
    [(txexpr 'pitch '()
       (list (txexpr 'step '() (leaf/str name))
             (txexpr 'octave '() (leaf/num octave))))
     (natural-pitch->note name octave)]
    [(txexpr 'pitch '()
       (list (txexpr 'step '() (leaf/str name))
             (txexpr 'alter '() (leaf/num alter))
             (txexpr 'octave '() (leaf/num octave))))
     (data/note-alteration+ (natural-pitch->note name octave) alter)]))

;; natural-pitch->note : String Int -> Note
(define (natural-pitch->note name octave)
  (match name
    ["C" (data/C octave)]
    ["D" (data/D octave)]
    ["E" (data/E octave)]
    ["F" (data/F octave)]
    ["G" (data/G octave)]
    ["A" (data/A octave)]
    ["B" (data/B octave)]))

;; ------------------------------------------------------------------------

;; attributes-element->muselements :
;; MXexpr Position -> [Listof MusElement]
(define (attributes-element->muselements mx pos)
  (match mx
    [(txexpr 'clef '()
       (list (txexpr 'sign '() (leaf/str sign))
             (txexpr 'line '() (leaf/num line))))
     (match* [sign line]
       [["G" 2] (list (data/timed/pos pos data/TREBLE-CLEF))]
       [["F" 4] (list (data/timed/pos pos data/BASS-CLEF))]
       [["C" 3] (list (data/timed/pos pos data/ALTO-CLEF))])]
    [(txexpr 'key '()
       (list (txexpr 'fifths '() (leaf/num fifths))))
     (list
      (data/timed/pos pos (data/key fifths)))]
    [(txexpr 'time '()
       (list (txexpr 'beats '() (leaf/num beats))
             (txexpr 'beat-type '() (leaf/str type))))
     (list
      (data/timed/pos pos
        (data/time-sig/nd
         beats
         (match type
           ["1" data/duration-whole]
           ["2" data/duration-half]
           ["4" data/duration-quarter]
           ["8" data/duration-eighth]
           ["16" data/duration-sixteenth]))))]
    [(txexpr tag _ _)
     (printf "TODO convert attributes tag: ~v\n~v\n" tag mx)
     '()]))

;; ------------------------------------------------------------------------

(module+ test
  (define example
    '(score-partwise
      ((version "3.0"))
      (work (work-title "Example Work"))
      (movement-title "Example Movement")
      (identification
       (creator ((type "composer")) "Example Composer"))
      (part-list (score-part ((id "P1")) (part-name "Music")))
      (part
       ((id "P1"))
       (measure
        ((number "1"))
        (attributes
         (divisions "2")
         (clef (sign "G") (line "2"))
         (key (fifths "0"))
         (time (beats "4") (beat-type "4")))
        (note (rest) (duration "2"))
        (note (pitch (step "C") (octave "4")) (duration "2") (type "quarter"))
        (note (pitch (step "D") (octave "4")) (duration "2") (type "quarter"))
        (note (pitch (step "E") (octave "4")) (duration "2") (type "quarter")))
       (measure
        ((number "2"))
        (note (pitch (step "F") (octave "4")) (duration "2") (type "quarter"))
        (note (pitch (step "E") (octave "4")) (duration "1") (type "eighth"))
        (note (pitch (step "D") (octave "4")) (duration "1") (type "eighth"))
        (note (pitch (step "E") (octave "4")) (duration "4") (type "half"))))))

  (check-equal?
    (musicxml->score example)
    (data/score
     (data/metadata
      (data/work "Example Work")
      #f
      "Example Movement"
      (data/creator "Example Composer"))
     (list
      (data/part
       "Music"
       (sorted/position
        (timed/pos (position 0 beat-one)
                   data/TREBLE-CLEF)
        (timed/pos (position 0 beat-one)
                   (data/key 0))
        (timed/pos (position 0 beat-one)
                   (time-sig/nd 4 duration-quarter))
        (timed (time-period (position 0 (duration 2 2)) (duration 2 2))
               C4)
        (timed (time-period (position 0 (duration 4 2)) (duration 2 2))
               D4)
        (timed (time-period (position 0 (duration 6 2)) (duration 2 2))
               E4)
        (timed (time-period (position 1 (duration 0 1)) (duration 2 2))
               F4)
        (timed (time-period (position 1 (duration 2 2)) (duration 1 2))
               E4)
        (timed (time-period (position 1 (duration 3 2)) (duration 1 2))
               D4)
        (timed (time-period (position 1 (duration 4 2)) (duration 4 2))
               E4))))))

  (define (equal~? a b)
    (match* [a b]
      [[(data/duration an ad) (data/duration bn bd)]
       (= (/ an ad) (/ bn bd))]
      [[_ _]
       (equal?/recur a b equal~?)]))

  (check equal~?
         (musicxml->score
          (score->musicxml
           Bach-Goldberg-Canone-alla-Quarta))
         Bach-Goldberg-Canone-alla-Quarta)
  )

;; ------------------------------------------------------------------------

