#lang agile

(require music-theory/util/txexpr
         (only-in srfi/1 append-reverse)
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
       (musicxml-measures->muselements
        (state (data/position 0 data/beat-one) #f 1 #f '())
        measures)))]))

;; --------------------------------------------------------------

;; A State is a
;; (state Position [Maybe TimePeriod] PosInt TimeSig Ties)
(struct state [pos chord-tp div time-sig ties] #:transparent)

;; A Ties is a [Listof TieState]
;; A TieState is a (tie-state Position Duration Position Note)
(struct tie-state [start dur mid note] #:transparent)

;; st/measure-boundary : State Nat -> State
;; gives warnings if it doesn't match up
(define (st/measure-boundary st n)
  (match st
    [(state pos ctp div ts ties)
     ;; TODO: give warnings if it doesn't match up
     (state
      (pos/measure-boundary pos ts n)
      ctp
      div
      ts
      (for/list ([t (in-list ties)])
        (match t
          [(tie-state start dur mid nt)
           (tie-state start dur (pos/measure-boundary mid ts n) nt)])))]))

;; pos/measure-boundary : Position TimeSig Nat -> Pos
(define (pos/measure-boundary pos ts n)
  (define pos-n (data/position n data/beat-one))
  (cond
    [ts
     (define meas-dur (data/time-sig-measure-length ts))
     (define pos* (data/roll-over-measure pos meas-dur))
     ;; warning if pos* doesn't match up with pos-n
     (unless (data/position=? pos* pos-n)
       (eprintf "warning: pos/measure-boundary\n"))
     pos-n]
    [else
     pos-n]))

;; --------------------------------------------------------------

;; musicxml-measures->muselements :
;; State [Listof MXexpr] -> [Listof MusElement]
(define (musicxml-measures->muselements st ms)
  (match ms
    ['() '()]
    [(cons m ms)
     (define-values [st* meas-elems]
       (musicxml-measure->muselements st m))
     (append
      meas-elems
      (musicxml-measures->muselements st* ms))]))

;; musicxml-measure->muselements :
;; State MXexpr -> (values State [Listof MusElement])
;; Order in the result list doesn't matter
(define (musicxml-measure->muselements st m)
  (match m
    [(txexpr 'measure attrs elements)
     (define mn (sub1 (string->number (attr-ref m 'number))))
     (musicxml-elements->muselements
      (st/measure-boundary st mn)
      elements)]))

;; musicxml-elements->muselements :
;; State [Listof MXexpr] -> (values State [Listof MusElement])
;; Order in the result list doesn't matter
(define (musicxml-elements->muselements st mxs)
  (match-define (state pos ctp div ts ties) st)
  (match mxs
    ['() (values st '())]
    [(cons fst rst)
     (match fst
       [(txexpr 'attributes '()
          (list (txexpr 'divisions '() (leaf/num div))
                other
                ...))
        (musicxml-elements->muselements
          (struct-copy state st [div div])
          (cons (txexpr 'attributes '() other) rst))]
       [(txexpr 'attributes '()
          (list (and elements (not (txexpr 'divisions _ _))) ...))
        (define-values [st* reves]
          (for/fold ([st st] [reves '()])
                    ([elem (in-list elements)])
            (define-values [st* es]
              (attributes-element->muselements st elem))
            (values st* (append-reverse es reves))))
        (define-values [st** elems-rst]
          (musicxml-elements->muselements st* rst))
        (values
         st**
         (append-reverse reves elems-rst))]
       [(txexpr 'backup '()
          (list (txexpr 'duration '() (leaf/num dur))))
        (define st*
          (struct-copy state st
            [pos (data/position- pos (data/duration dur div))]))
        (musicxml-elements->muselements st rst)]
       [(txexpr 'forward '()
          (list (txexpr 'duration '() (leaf/num dur))))
        (define st*
          (struct-copy state st
            [pos (data/position+ pos (data/duration dur div))]))
        (musicxml-elements->muselements st* rst)]
       [(txexpr 'harmony _ _)
        ('....)]
       [(txexpr 'direction _ _)
        (define-values [st* elems]
          (musicxml-direction->muselements st fst))
        (define-values [st** elems-rst]
          (musicxml-elements->muselements st* rst))
        (values
         st**
         (append elems elems-rst))]
       [(txexpr 'note _ _)
        (define-values [st* elems]
          (musicxml-note->muselements st fst))
        (define-values [st** elems-rst]
          (musicxml-elements->muselements st* rst))
        (values
         st**
         (append elems elems-rst))])]))

;; musicxml-direction->muselements :
;; State MXexpr -> (values State [Listof MusElement])
(define (musicxml-direction->muselements st mx)
  (match-define (state pos _ _ _ _) st)
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
     (values
      st
      (list
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
           )))))]))

;; musicxml-note->muselements :
;; State MXexpr -> (values State [Listof MusElement])
(define (musicxml-note->muselements st note)
  (match-define (state pos ctp div ts ties) st)
  (match note
    [(txexpr 'note attrs
       (list (txexpr 'rest '() _)
             (txexpr 'duration '() (leaf/num dur))
             (or (txexpr 'voice _ _)
                 (txexpr 'staff _ _)
                 (txexpr 'type _ _)
                 (txexpr 'dot _ _))
             ...))
     (define d (data/duration dur div))
     (values
      (state
       (data/position+ pos d)
       ;; TODO: can a note be in a chord with a rest?
       (data/time-period pos d)
       div
       ts
       ;; TODO: handle ties
       ties)
      '())]
    [(txexpr 'note attrs
       (list (txexpr 'pitch _ _)
             (txexpr 'duration _ _)
             (not (txexpr 'rest _ _)
                  (txexpr 'chord _ _))
             ...))
     (define nt (musicxml->note-there st note))
     (values
      (state
       (data/position+ pos (data/note-there-duration nt))
       (data/timed-period nt)
       div
       ts
       ;; TODO: handle ties
       ties)
      (list nt))]
    [(txexpr 'note attrs
       (list (txexpr 'chord '() '())
             others
             ...))
     (define chord-pos (data/time-period-start ctp))
     (define nt
       (musicxml->note-there (struct-copy state st [pos chord-pos])
                             (txexpr 'note '() others)))
     (unless (equal? (data/timed-period nt) ctp)
       (error 'chord "notes not same duration"))
     (values
      st
      (list nt))]))

;; musicxml->note-there : State MXexpr -> NoteThere
(define (musicxml->note-there st note)
  (match-define (state pos _ div ts ties) st)
  (match note
    [(txexpr 'note attrs
       (list (and pitch (txexpr 'pitch _ _))
             (txexpr 'duration '() (leaf/num dur))
             (or (txexpr 'voice _ _)
                 (txexpr 'type _ _)
                 (txexpr 'dot _ _)
                 (txexpr 'notations _
                   (list (or (txexpr 'tied _ _))
                         ...)))
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
;; State MXexpr -> (values State [Listof MusElement])
(define (attributes-element->muselements st mx)
  (match-define (state pos _ _ _ _) st)
  (match mx
    [(txexpr 'clef '()
       (list (txexpr 'sign '() (leaf/str sign))
             (txexpr 'line '() (leaf/num line))))
     (values
      st
      (match* [sign line]
        [["G" 2] (list (data/timed/pos pos data/TREBLE-CLEF))]
        [["F" 4] (list (data/timed/pos pos data/BASS-CLEF))]
        [["C" 3] (list (data/timed/pos pos data/ALTO-CLEF))]))]
    [(txexpr 'key '()
       (list (txexpr 'fifths '() (leaf/num fifths))))
     (values
      st
      (list
       (data/timed/pos pos (data/key fifths))))]
    [(txexpr 'time '()
       (list (txexpr 'beats '() (leaf/num beats))
             (txexpr 'beat-type '() (leaf/str type))))
     (values
      st
      (list
       (data/timed/pos pos
         (data/time-sig/nd
          beats
          (match type
            ["1" data/duration-whole]
            ["2" data/duration-half]
            ["4" data/duration-quarter]
            ["8" data/duration-eighth]
            ["16" data/duration-sixteenth])))))]
    [(txexpr tag _ _)
     (printf "TODO convert attributes tag: ~v\n~v\n" tag mx)
     (values
      st
      '())]))

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

