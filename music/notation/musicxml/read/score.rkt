#lang agile

(require music/util/txexpr
         racket/pretty
         (only-in unstable/match as)
         "musicxml-file.rkt"
         "metadata.rkt"
         (prefix-in data/
           (combine-in
            music/data/time/main
            music/data/note/main
            music/data/score/main)))
(module+ test
  (require rackunit
           music/data/time/main
           (submod music/data/note/note example)
           (submod music/data/score/score example)
           music/example/Bach-Goldberg-Canone-alla-Quarta
           "../score.rkt"))

;; replaces x with y in the list lst (once)
(define (replace x y lst)
  (match lst
    ['()               (cons y '())]
    [(cons (== x) rst) (cons y rst)]
    [(cons a rst)      (cons a (replace x y rst))]))

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
  (for/list ([part-entry (in-list part-entries)]
             #:unless (part-group-element? part-entry))
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
       (musicxml->part name part)]
      #;[_
       (pretty-write part-entry)
       (error 'bad)])))

(define (part-group-element? mx)
  (match mx
    [(txexpr 'part-group _ _) #true]
    [_ #false]))

;; musicxml->part : String MXexpr -> Part
(define (musicxml->part name part)
  (match part
    [(txexpr 'part _ (list (and measures (txexpr 'measure _ _)) ...))
     (data/part
      name
      (data/sorted/time-period
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
     (unless (or (zero? n) ts)
       (eprintf
        (string-append
         "warning: reached a measure boundary with no time signature\n"
         "  measure: ~v\n")
        n))
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
    ['() (map tie-done->note-there (state-ties st))]
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
  (match mxs
    ['() (values st '())]
    [(cons fst rst)
     (define-values [st* fst-es]
       (musicxml-element->muselements st fst))
     (define-values [st** rst-es]
       (musicxml-elements->muselements st* rst))
     (values
      st**
      (append fst-es rst-es))]))

;; musicxml-element->muselements :
;; State MXexpr -> (values State [Listof MusElement])
(define (musicxml-element->muselements st fst)
  (match-define (state pos ctp div ts ties) st)
  (match fst
    [(txexpr 'print _ _)
     (printf "TODO: musicxml tag print\n(currently a no-op)\n")
     ;; a no-op
     (values st '())]
    [(txexpr 'barline _ _)
     ;; a no-op
     (values st '())]
    [(txexpr 'attributes '()
       (list elements ...))
     (attributes-elements->muselements
       st
       elements)]
    [(txexpr 'backup '()
       (list (txexpr 'duration '() (leaf/num dur))))
     (values
      (struct-copy state st
        [pos (data/position- pos (data/duration dur div))])
      '())]
    [(txexpr 'forward '()
       (list (txexpr 'duration '() (leaf/num dur))))
     (values
      (struct-copy state st
        [pos (data/position+ pos (data/duration dur div))])
      '())]
    [(txexpr 'harmony _ _)
     ('....)]
    [(txexpr 'direction _ _)
     (musicxml-direction->muselements st fst)]
    [(txexpr 'note _ _)
     (musicxml-note->muselements st fst)]))

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
           )))))]
    [(txexpr 'direction _ _)
     (printf "TODO: musicxml direction tag\n(currently a no-op)\n")
     ;; a no-op
     (values st '())]))

;; musicxml-note->muselements :
;; State MXexpr -> (values State [Listof MusElement])
(define (musicxml-note->muselements st note)
  (match-define (state pos ctp div ts ties) st)
  (match note
    [(txexpr 'note attrs
       (list (txexpr 'rest '() _)
             (and dur-mx (txexpr 'duration _ _))
             (or (txexpr 'voice _ _)
                 (txexpr 'staff _ _)
                 (txexpr 'type _ _)
                 (txexpr 'dot _ _)
                 (txexpr 'time-modification _ _) ;; TODO: what does this do?
                 (txexpr 'accidental _ _) ;; TODO: is this safe to ignore?
                 (txexpr 'stem _ _)
                 (txexpr 'beam _ _)
                 (txexpr 'lyric _ _)
                 (txexpr 'syllabic _ _)
                 (txexpr 'text _ _)
                 (txexpr 'notations _ _))
             ...))
     (define dur (musicxml-duration->duration st dur-mx))
     (define tp (data/time-period pos dur))
     (values
      (struct-copy state st
        [pos (data/time-period-end tp)]
        ;; TODO: can a note be in a chord with a rest?
        [chord-tp tp])
      '())]
    [(txexpr 'note attrs
       (list (and pitch-mx (txexpr 'pitch _ _))
             (and dur-mx (txexpr 'duration _ _))
             (and tie-stuff (txexpr 'tie _ _))
             ...
             (and (or (txexpr 'voice _ _)
                      (txexpr 'staff _ _)
                      (txexpr 'type _ _)
                      (txexpr 'dot _ _)
                      (txexpr 'time-modification _ _) ;; TODO: what does this do?
                      (txexpr 'accidental _ _) ;; TODO: is this safe to ignore?
                      (txexpr 'stem _ _)
                      (txexpr 'beam _ _)
                      (txexpr 'lyric _ _)
                      (txexpr 'syllabic _ _)
                      (txexpr 'text _ _)
                      (txexpr 'notations _ _))
                  (not (txexpr 'rest _ _)
                       (txexpr 'chord _ _)))
             ...))
     (define n (musicxml-pitch->note pitch-mx))
     (define dur (musicxml-duration->duration st dur-mx))
     (define tp (data/time-period pos dur))
     (define end (data/time-period-end tp))
     ;; TODO: handle ties by using tie-stuff
     (match tie-stuff
       [(list)
        (values
         (struct-copy state st [pos end] [chord-tp tp])
         (list (data/timed tp n)))]
       [(list (txexpr 'tie '([type "start"]) '()))
        (values
         (struct-copy state st [pos end] [chord-tp tp]
           [ties
            (cons (tie-state pos dur end n)
                  ties)])
         '())]
       [(list (txexpr 'tie '([type "stop"]) '()))
        (define t (find-tie ties pos n))
        (values
         (struct-copy state st [pos end] [chord-tp tp]
           [ties
            (remove t ties)])
         (list
          (tie-done->note-there (extend-tie t dur))))]
       [(list-no-order (txexpr 'tie '([type "start"]) '())
                       (txexpr 'tie '([type "stop"]) '()))
        (define t (find-tie ties pos n))
        (values
         (struct-copy state st [pos end] [chord-tp tp]
           [ties
            (replace t (extend-tie t dur) ties)])
         '())]
       )]

    ;; for a chord element, it resets the current pos back to the
    ;; chord's position the state after the chord element should
    ;; be unchanged, so the time periods must be equal.
    [(txexpr 'note attrs
       (list (txexpr 'chord '() '())
             (and pitch-mx (txexpr 'pitch _ _))
             (and dur-mx (txexpr 'duration _ _))
             others
             ...))
     (define chord-pos (data/time-period-start ctp))
     (define chord-st (struct-copy state st [pos chord-pos]))

     (define dur (musicxml-duration->duration st dur-mx))
     (define tp (data/time-period chord-pos dur))
     ;; need this check otherwise the next state might be inconsistent
     (unless (equal? tp ctp)
       (error 'chord "notes not same duration"))

     (musicxml-note->muselements
      chord-st
      (txexpr 'note attrs (list* pitch-mx dur-mx others)))]))

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

;; musicxml-duration->duration : State MXexpr -> Duration
(define (musicxml-duration->duration st d)
  (match d
    [(txexpr 'duration '() (leaf/num dur-n))
     (data/duration dur-n (state-div st))]))

;; ------------------------------------------------------------------------

;; find-tie : [Listof TieState] Position Note -> TieState
(define (find-tie ties pos n)
  (or (findf (tie-matches? pos n) ties)
      (and (eprintf "no start tie to match stop tie. pos: ~v\n" pos)
           (tie-state pos data/duration-zero pos n))))

;; extend-tie : TieState Duration -> TieState
(define (extend-tie t ∆dur)
  (match t
    [(tie-state pos dur end n)
     (tie-state pos
                (data/duration+ dur ∆dur)
                (data/position+ end ∆dur)
                n)]))

;; tie-done->note-there : TieState -> NoteThere
(define (tie-done->note-there t)
  (match t
    [(tie-state pos dur end n)
     (data/timed (data/time-period pos dur) n)]))

;; tie-matches? : Position Note -> [TieState -> Boolean]
(define ((tie-matches? pos n) t)
  (match-define (tie-state _ _ t-end t-n) t)
  (and (data/position=? pos t-end)
       (data/note=? n t-n)))

;; ------------------------------------------------------------------------

;; attributes-elements->muselements :
;; State [Listof MXexpr] -> (values State [Listof MusElement])
(define (attributes-elements->muselements st elements)
  (match elements
    ['() (values st '())]
    [(cons elem rst)
     (define-values [st* es]
       (attributes-element->muselements st elem))
     (define-values [st** rst-es]
       (attributes-elements->muselements st* rst))
     (values
      st**
      (append es rst-es))]))

;; attributes-element->muselements :
;; State MXexpr -> (values State [Listof MusElement])
(define (attributes-element->muselements st mx)
  (match-define (state pos _ _ _ _) st)
  (match mx
    [(txexpr 'divisions '() (leaf/num div))
     (values
      (struct-copy state st [div div])
      '())]
    [(txexpr 'clef '()
       (list-rest
        (txexpr 'sign '() (leaf/str sign))
        (txexpr 'line '() (leaf/num line))
        (or (as ([octave 0]) '())
            (list (txexpr 'clef-octave-change '() (leaf/num octave))))))
     (values
      st
      (list
       (data/timed/pos
        pos
        (data/clef-shift-octave (sign+line->clef sign line) octave))))]
    [(txexpr 'key '()
       (list (txexpr 'fifths '() (leaf/num fifths))))
     (values
      st
      (list
       (data/timed/pos pos (data/key fifths))))]
    [(txexpr 'time '()
       (list (txexpr 'beats '() (leaf/num beats))
             (txexpr 'beat-type '() (leaf/str type))))
     (define ts
       (data/time-sig/nd
        beats
        (match type
          ["1" data/duration-whole]
          ["2" data/duration-half]
          ["4" data/duration-quarter]
          ["8" data/duration-eighth]
          ["16" data/duration-sixteenth])))
     (values
      (struct-copy state st [time-sig ts])
      (list
       (data/timed/pos pos ts)))]
    [(txexpr tag _ _)
     (printf "TODO convert attributes tag: ~v\n~v\n" tag mx)
     (values
      st
      '())]))

(define (sign+line->clef sign line)
  (match* [sign line]
    [["G" 2] data/TREBLE-CLEF]
    [["F" 4] data/BASS-CLEF]
    [["C" 3] data/ALTO-CLEF]))

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
           SIMPLE-EXAMPLE))
         SIMPLE-EXAMPLE)

  (check equal~?
         (musicxml->score
          (score->musicxml
           CHANGING-TIME-SIG))
         CHANGING-TIME-SIG)

  (check equal~?
         (musicxml->score
          (score->musicxml
           Bach-Goldberg-Canone-alla-Quarta))
         Bach-Goldberg-Canone-alla-Quarta)
  )

;; ------------------------------------------------------------------------

