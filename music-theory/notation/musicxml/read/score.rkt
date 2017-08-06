#lang agile

(require (rename-in (submod txexpr safe) [txexpr -txexpr])
         "musicxml-file.rkt"
         (prefix-in data/
           (combine-in
            "../../../data/note.rkt"
            "../../../data/note-held.rkt"
            "../../../data/position.rkt"
            "../../../data/score/score.rkt"
            "../../../data/score/metadata.rkt"
            "../../../data/score/key-signature.rkt"
            "../../../data/score/time-signature.rkt"
            "../../../data/score/tempo.rkt")))

(define-match-expander txexpr
  (syntax-parser
    [(_ tag-pat:expr attrs-pat:expr elements-pat:expr)
     #'(? txexpr? (app txexpr->values tag-pat attrs-pat elements-pat))])
  (syntax-parser
    [:id #'-txexpr]
    [(_ tag:expr attrs:expr elements:expr)
     #'(-txexpr tag attrs elements)]))

;; str-leaf : [Listof String] -> String
(define (str-leaf elements)
  (apply string-append elements))

;; num-leaf : [Listof String] -> [Maybe Number]
(define (num-leaf elements)
  (string->number (str-leaf elements)))

;; ------------------------------------------------------------------------

;; musicxml->score : MXexpr -> Score
(define (musicxml->score mx)
  (match mx
    [(txexpr 'score-partwise '([version "3.0"])
       (list
        (and metadata-elements (not (txexpr 'part-list _ _)
                                    (txexpr 'part _ _)))
        ...
        (and part-list (txexpr 'part-list _ _))
        (and parts (txexpr 'part _ _))
        ...))
     (data/score
      ; TODO: use metadata-elements
      #false
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
         (list (txexpr 'part-name '()
                 (list (? string? name-elements) ...))))
       (define name (str-leaf name-elements))
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
    ['() acc]
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
          (list (txexpr 'divisions '() (list (? string? divs) ...))
                other
                ...))
        (musicxml-elements->muselements
          (cons (txexpr 'attributes '() other) rst)
          rst-measures
          pos
          (num-leaf divs)
          acc)]
       [(txexpr 'attributes '()
          (list (and elements (not (txexpr 'divisions _ _))) ...))
        (musicxml-elements->muselements rst rst-measures pos div
          (for/fold ([acc acc])
                    ([elem (in-list elements)])
            (append (attributes-element->muselements elem pos)
                    acc)))]
       [(txexpr 'backup '()
          (list (txexpr 'duration '() (list (? string? durs) ...))))
        (musicxml-elements->muselements rst rst-measures
          (data/position- pos (data/duration (num-leaf durs) div))
          div
          acc)]
       [(txexpr 'forward '()
          (list (txexpr 'duration '() (list (? string? durs) ...))))
        (musicxml-elements->muselements rst rst-measures
          (data/position+ pos (data/duration (num-leaf durs) div))
          div
          acc)]
       [(txexpr 'harmony _ _)
        ('....)]
       [(txexpr 'note _ _)
        (musicxml-note->muselements fst rst rst-measures pos div acc)])]))

;; musicxml-note->muselements :
;; MXexpr [Listof MXexpr] [Listof MXexpr] Position Nat [Listof MusElement]
;; -> [Listof MusElement]
(define (musicxml-note->muselements note rst rst-measures pos div acc)
  (match note
    [(txexpr 'note '()
       (list (txexpr 'rest '() '())
             (txexpr 'duration '() (list (? string? durs) ...))))
     (musicxml-elements->muselements rst rst-measures
       (data/position+ pos (data/duration (num-leaf durs) div))
       div
       acc)]
    [(txexpr 'note '()
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
    [(txexpr 'note '()
       (list (txexpr 'chord '() '())
             others
             ...))
     ('....)]))

;; musicxml->note-there : MXexpr Position Nat -> NoteThere
(define (musicxml->note-there note pos div)
  (match note
    [(txexpr 'note '()
       (list (and pitch (txexpr 'pitch _ _))
             (txexpr 'duration '() (list (? string? durs) ...))
             (or (txexpr 'type _ _)
                 (txexpr 'dot _ _))
             ...))
     (data/with-pos pos
       (data/note-held (musicxml-pitch->note pitch)
                       (data/duration (num-leaf durs) div)))]))

;; musicxml-pitch->note : MXexpr -> Note
(define (musicxml-pitch->note pitch)
  (match pitch
    [(txexpr 'pitch '()
       (list (txexpr 'step '() (list (? string? name-strs) ...))
             (txexpr 'octave '() (list (? string? octave-strs) ...))))
     (natural-pitch->note (str-leaf name-strs) (num-leaf octave-strs))]
    [(txexpr 'pitch '()
       (list (txexpr 'step '() (list (? string? name-strs) ...))
             (txexpr 'alter '() (list (? string? alter-strs) ...))
             (txexpr 'octave '() (list (? string? octave-strs) ...))))
     (data/note-alteration+
      (natural-pitch->note (str-leaf name-strs) (num-leaf octave-strs))
      (num-leaf alter-strs))]))

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
    [(txexpr 'key '()
       (list (txexpr 'fifths '() (list (? string? fifths-strs) ...))))
     (list
      (data/with-pos pos
        (data/key (num-leaf fifths-strs))))]
    [(txexpr 'time '()
       (list (txexpr 'beats '() (list (? string? beats-strs) ...))
             (txexpr 'beat-type '() (list (? string? type-strs) ...))))
     (list
      (data/with-pos pos
        (data/time-sig/nd
         (num-leaf beats-strs)
         (match (str-leaf type-strs)
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
      (part-list (score-part ((id "P1")) (part-name "Music")))
      (part
       ((id "P1"))
       (measure
        ((number "1"))
        (attributes
         (divisions "2")
         (key (fifths "0"))
         (time (beats "4") (beat-type "4"))
         (clef (sign "G") (line "2")))
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

  (musicxml->score example))

;; ------------------------------------------------------------------------

