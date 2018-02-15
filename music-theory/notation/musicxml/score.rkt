#lang agile

;; The main function of this file is score->musicxml

(require racket/format
         (submod txexpr safe)
         "musicxml-file.rkt"
         "metadata.rkt"
         "harmony-element.rkt"
         "clef.rkt"
         (prefix-in data/
           (combine-in
            music-theory/data/time/main
            music-theory/data/time/measures
            music-theory/data/note/main
            music-theory/data/score/main)))
(module+ test
  (provide SIMPLE-EXAMPLE/MusicXML CHANGING-TIME-SIG/MusicXML)
  (require rackunit
           (submod music-theory/data/score/score example)))
(module+ demo
  (require racket/runtime-path
           racket/pretty
           (submod music-theory/data/score/score example)
           (submod ".." test)))

;; A MXexpr is a TXexpr in MusicXML format

;; ------------------------------------------------------------------------

;; %partwise

(define (score-partwise #:version version-str . elements)
  (txexpr 'score-partwise `([version ,version-str]) elements))

;; ------------------------------------------------------------------------

;; %score-header

;; See also metadata.rkt

(define (part-list . elements)
  (txexpr 'part-list '() elements))

(define (score-part #:id part-id-str . elements)
  (txexpr 'score-part `([id ,part-id-str]) elements))

(define (part-name . elements)
  (txexpr 'part-name '() elements))

;; ------------------------------------------------------------------------

(define (part #:id part-id-str . elements)
  (txexpr 'part `([id ,part-id-str]) elements))

(define (measure #:number number-str . elements)
  (txexpr 'measure `([number ,number-str]) elements))

;; ------------------------------------------------------------------------

;; %music-data

(define (attributes . elements)
  (txexpr 'attributes '() elements))

(define (divisions . elements)
  (txexpr 'divisions '() elements))

(define (key #:fifths fifths-str)
  (txexpr 'key '() (list (txexpr 'fifths '() (list fifths-str)))))

(define (time #:beats beats-str #:beat-type beat-type-str)
  (txexpr 'time '()
    (list
     (txexpr 'beats '() (list beats-str))
     (txexpr 'beat-type '() (list beat-type-str)))))

;; Musical directions used for expression marks, such as tempo, style,
;; dynamics, etc.
(define (direction #:placement placement-str . elements)
  (txexpr 'direction `([placement ,placement-str])
    elements))

(define (direction-type . elements)
  (txexpr 'direction-type '() elements))

(define (metronome . elements)
  (txexpr 'metronome '() elements))

(define (beat-unit beat-unit-str)
  (txexpr 'beat-unit '() (list beat-unit-str)))

(define (beat-unit-dot)
  (txexpr 'beat-unit-dot '() '()))

(define (per-minute per-minute-str)
  (txexpr 'per-minute '() (list per-minute-str)))

(define (sound #:tempo tempo-str)
  (txexpr 'sound `([tempo ,tempo-str]) '()))

(define (note . elements)
  (txexpr 'note '() elements))

(define (rest)
  (txexpr 'rest '() '()))

(define (chord)
  (txexpr 'chord '() '()))

(define (backup . elements)
  (txexpr 'backup '() elements))

(define (pitch . elements)
  (txexpr 'pitch '() elements))

(define (step . elements)
  (txexpr 'step '() elements))

(define (alter . elements)
  (txexpr 'alter '() elements))

(define (octave . elements)
  (txexpr 'octave '() elements))

(define (duration . elements)
  (txexpr 'duration '() elements))

(define (tie #:type start-stop)
  (txexpr 'tie `([type ,start-stop]) '()))

(define (voice . elements)
  (txexpr 'voice '() elements))

(define (type . elements)
  (txexpr 'type '() elements))

(define (dot)
  (txexpr 'dot '() '()))

(define (notations . elements)
  (txexpr 'notations '() elements))

(define (tied #:type start-stop)
  (txexpr 'tied `([type ,start-stop]) '()))

;; ------------------------------------------------------------------------

;; A MaybeTied is one of:
;;  - (list NoteThere)
;;  - (list NoteThere TieCont)
(define (mt-single? mt) (= 1 (length mt)))
(define (mt-tied? mt) (< 1 (length mt)))

;; A TieCont is a NoteThere
;; where it starts at the beginning of a measure
;; It might be the end, or it might be the middle, check by
;; splitting it in the current time signature, then use
;; mt-single? and mt-tied?

;; A TieNote is a (tie-note Bool Bool Note)
(struct tie-note [start? end? value])

;; tie-note-single,
;; tie-note-start,
;; tie-note-end,
;; tie-note-mid : Note -> TieNote
(define (tie-note-single n) (tie-note #f #f n))
(define (tie-note-start n)  (tie-note #t #f n))
(define (tie-note-end n)    (tie-note #f #t n))
(define (tie-note-mid n)    (tie-note #t #t n))

;; A TieNoteThere is a [Timed TieNote]
(define (tie-note-there? v)
  (and (data/timed? v)
       (tie-note? (data/timed-value v))
       (data/note? (tie-note-value (data/timed-value v)))))

;; tie-note-there-single,
;; tie-note-there-start,
;; tie-note-there-end,
;; tie-note-there-mid : NoteThere -> TieNoteThere
(define (tie-note-there-single nt) (data/timed-map nt tie-note-single))
(define (tie-note-there-start nt)  (data/timed-map nt tie-note-start))
(define (tie-note-there-end nt)    (data/timed-map nt tie-note-end))
(define (tie-note-there-mid nt)    (data/timed-map nt tie-note-mid))

;; elems-split-over-measure/no-tie : SortedNotes -> [Listof MaybeTied]
(define (elems-split-over-measure/no-tie elems meas-dur)
  (map (λ (e)
         (data/timed-split-over-measure/no-tie e meas-dur))
       elems))

;; ------------------------------------------------------------------------

;; A State is a (state Position PosInt)
(struct state
  [position divisions]
  #:transparent)

;; st+meas : State -> State
(define (st+meas s)
  (match s
    [(state pos div)
     (state (data/position-measure+ pos 1) div)]))

;; st+dur : State Duration -> State
(define (st+dur s d)
  (match s
    [(state pos div)
     (state (data/position+ pos d) div)]))

;; st/measure-boundary : State TimeSig -> State
(define (st/measure-boundary s ts)
  (match s
    [(state (data/position n p) div)
     (unless (data/duration=? p (data/time-sig-measure-length ts))
       (error 'st/measure-boundary "not at measure boundary"))
     (state (data/position (add1 n) data/duration-zero) div)]))

;; ------------------------------------------------------------------------

(provide score->musicxml)

;; score->musicxml : Score -> MXexpr
(define (score->musicxml s)
  (match s
    [(data/score metadata parts)
     (apply score-partwise
       #:version "3.0"
       (append
        (metadata->musicxml-elements metadata)
        (cons
         (part-list->musicxml parts)
         (parts->musicxml-elements parts))))]))

(define (part-id i)
  (format "P~a" i))

;; part-list->musicxml : [Listof Part] -> MXexpr
(define (part-list->musicxml parts)
  (apply part-list
    (for/list ([p (in-list parts)]
               [i (in-naturals 1)])
      (score-part #:id (part-id i)
        (part-name (data/part-name p))))))

;; parts->musicxml-elements :
;; [Listof Part] -> [Listof MXexpr]
(define (parts->musicxml-elements parts)
  (for/list ([p (in-list parts)]
             [i (in-naturals 1)])
    (part->musicxml p i)))

;; part->musicxml : Part Nat -> MXexpr
(define (part->musicxml p i)
  (match p
    [(data/part _ sorted-notes)
     (apply part #:id (part-id i)
       (muselems->musicxml-elements sorted-notes))]))

;; muselems->musicxml-elements :
;; SortedNotes -> [Listof MXexpr]
(define (muselems->musicxml-elements sorted-notes)
  (define div
    (apply data/duration-common-divisions
      (for*/list ([nt (in-list sorted-notes)]
                  #:when (data/note-there? nt))
        (data/note-there-duration nt))))

  (define init-s
    (state (data/position 0 data/duration-zero) div))

  (define measures (data/group-measures sorted-notes))

  (measures->musicxml-elements measures '() init-s))

;; ------------------------------------------------------------------------

;; measures->musicxml-elements :
;; [Listof Measure] [Listof TieCont] State -> [Listof MXexpr]
(define (measures->musicxml-elements ms ties st)
  (match ms
    ['() '()]
    [(cons fst rst)
     (define ts (data/measure-time-sig fst))
     (define-values [m ties* st*]
       (measure->musicxml fst ties st))
     (cons
      m
      (measures->musicxml-elements rst ties* (st/measure-boundary st* ts)))]))

;; measure->musicxml :
;; SortedNotes [Listof TieCont] State
;; -> (values MXexpr [Listof TieCont] State)
(define (measure->musicxml m old-ties st)
  (match-define (data/measure ts elems) m)
  (match-define (state (data/position n _) div) st)
  (define meas-dur (data/time-sig-measure-length ts))
  (define meas-end (data/position n meas-dur))

  ;; old-tie-ends : [Listof NoteThere]
  ;; old-tie-mids : [Listof NoteThere]
  ;; old-tie-conts : [Listof TieCont]
  (match-define-values [(list (list old-tie-ends) ...)
                        (list (list old-tie-mids old-tie-conts) ...)]
    (partition mt-single? (elems-split-over-measure/no-tie old-ties meas-dur)))

  ;; single-notes : [Listof NoteThere]
  ;; tie-starts : [Listof NoteThere]
  ;; new-tie-conts : [Listof TieCont]
  (match-define-values [(list (list single-notes) ...)
                        (list (list tie-starts new-tie-conts) ...)]
    (partition mt-single? (elems-split-over-measure/no-tie elems meas-dur)))

  (define tie-notes
    (append
     (map tie-note-there-single single-notes)
     (map tie-note-there-start tie-starts)
     (map tie-note-there-mid old-tie-mids)
     (map tie-note-there-end old-tie-ends)))

  (define next-tie-conts
    (append old-tie-conts new-tie-conts))

  (define groups
    (group-by data/get-position tie-notes))

  (define number-str (number->string (add1 n)))
  (define div-str (number->string div))

  (define-values [st* mx-elems]
    (note-groups->musicxml-elements groups st))
  (define-values [st** meas-end-adj]
    (adjust-position->musicxml-elements st* meas-end))
  (define meas-elems
    (append mx-elems meas-end-adj))
  (cond
    [(zero? n)
     (values
      (apply measure #:number number-str
        (attributes
         (divisions div-str))
        meas-elems)
      next-tie-conts
      st**)]
    [else
     (values
      (apply measure #:number number-str
        meas-elems)
      next-tie-conts
      st**)]))

;; key->attribute-musicxml : Key -> MXexpr
(define (key->attribute-musicxml k)
  (key #:fifths (number->string (data/key-fifths k))))

;; time->attribute-musicxml : TimeSig -> MXexpr
(define (time->attribute-musicxml ts)
  (define-values [beats beat-type]
    (data/time-sig->nd-values ts))
  (time #:beats (number->string beats)
        #:beat-type
        (cond
          [(data/duration=? beat-type data/duration-whole) "1"]
          [(data/duration=? beat-type data/duration-half) "2"]
          [(data/duration=? beat-type data/duration-quarter) "4"]
          [(data/duration=? beat-type data/duration-eighth) "8"]
          [(data/duration=? beat-type data/duration-sixteenth) "16"]
          [else (error 'type->musicxml "given beat-type: ~v" beat-type)])))

;; tempo->direction-musicxml : Tempo -> MXexpr
(define (tempo->direction-musicxml t)
  (match-define (data/tempo bpm b) t)
  (define frac (data/duration-fraction b data/duration-quarter))
  (direction #:placement "above"
    (direction-type
     (apply metronome
       (append
        (cond
          [(data/duration=? b data/duration-whole)
           (list (beat-unit "whole"))]
          [(data/duration=? b data/duration-half)
           (list (beat-unit "half"))]
          [(data/duration=? b data/duration-quarter)
           (list (beat-unit "quarter"))]
          [(data/duration=? b data/duration-eighth)
           (list (beat-unit "eighth"))]
          [(data/duration=? b data/duration-sixteenth)
           (list (beat-unit "16th"))]
          [(data/duration=? b data/duration-dotted-whole)
           (list (beat-unit "whole") (beat-unit-dot))]
          [(data/duration=? b data/duration-dotted-half)
           (list (beat-unit "half") (beat-unit-dot))]
          [(data/duration=? b data/duration-dotted-quarter)
           (list (beat-unit "quarter") (beat-unit-dot))]
          [(data/duration=? b data/duration-dotted-eighth)
           (list (beat-unit "eighth") (beat-unit-dot))]
          [else (error 'tempo "given beat-type: ~v" b)])
        (list
         (per-minute (number->string bpm))))))
    (sound #:tempo (~r (* frac bpm)))))

;; note-groups->musicxml-elements :
;; [Listof [Listof TieNoteThere]] State
;; -> [Listof MXexpr]
(define (note-groups->musicxml-elements groups st)
  (match groups
    ['()
     (values st '())]
    [(cons fst rst)
     ;; make adjustments so that note-pos is the new state pos
     (define note-pos (data/get-position (first fst)))
     (define-values [note-st adj]
       (adjust-position->musicxml-elements st note-pos))

     (define-values [notes other-elements]
       (partition tie-note-there? fst))
     (define chords
       (group-by data/timed-duration notes data/duration=?))

     (define others
       (other-elements->musicxml-elements
        ;; TODO: What if some other musical element is "tied" over a barline?
        (map (λ (e) (data/timed-map e tie-note-value)) other-elements)))

     (define-values [st* mx-elems]
       (chords->musicxml-elements note-pos chords note-st))
     (define-values [st** rst-elems]
       (note-groups->musicxml-elements rst st*))
     (values
      st**
      (append adj others mx-elems rst-elems))]))

;; other-elements->musicxml-elements :
;; [Listof [WithPos MusElement]] -> [Listof MXexpr]
(define (other-elements->musicxml-elements es)
  (append*
   (for/list ([e (in-list es)])
     (match e
       [(data/timed _ (? data/clef? c))
        (list (attributes (clef->attribute-musicxml c)))]
       [(data/timed _ (? data/key? k))
        (list (attributes (key->attribute-musicxml k)))]
       [(data/timed _ (? data/time-sig? t))
        (list (attributes (time->attribute-musicxml t)))]
       [(data/timed _ (? data/tempo? t))
        (list (tempo->direction-musicxml t))]
       [(data/timed _ (? data/harmony-element? he))
        (list (harmony-element->musicxml he))]))))

;; adjust-position->musicxml-elements :
;; State Position -> (values State [Listof MXexpr])
(define (adjust-position->musicxml-elements st note-pos)
  (match-define (state pos div) st)
  (cond [(data/position=? pos note-pos)  (values st '())]
        [(data/position<? pos note-pos)
         (values
          (state note-pos div)
          (list (rest-duration->musicxml (data/position∆ pos note-pos) div)))]
        [else
         (values
          (state note-pos div)
          (list (backup-duration->musicxml (data/position∆ note-pos pos) div)))]
        ))

;; rest-duration->musicxml : Duration PosInt -> MXexpr
(define (rest-duration->musicxml d divisions)
  (define n (data/duration-n/divisions d divisions))
  (note (rest) (duration (number->string n))))

;; backup-duration->musicxml : Duration PosInt -> MXexpr
(define (backup-duration->musicxml d divisions)
  (define n (data/duration-n/divisions d divisions))
  (backup (duration (number->string n))))

;; chords->musicxml-elements :
;;   Position
;;   [Listof [NEListof TieNoteThere]]
;;   State
;;   ->
;;   (values State [Listof MXexpr])
(define (chords->musicxml-elements note-pos
                                   chords
                                   st)
  (match-define (state pos div) st)
  (match chords
    ['()
     (values st '())]
    [(cons fst rst)
     (define d (data/timed-duration (first fst)))

     ;; pos is now note-pos
     (define-values [note-st backup]
       (adjust-position->musicxml-elements st note-pos))

     (define-values [st* rst-elems]
       (chords->musicxml-elements note-pos rst (st+dur note-st d)))
     (values
      st*
      (append
       backup
       (for/list ([nt (in-list fst)]
                  [i (in-naturals)])
         (tie-note-there->musicxml nt note-st (not (zero? i))))
       rst-elems))]))

;; tie-note-there->musicxml : TieNoteThere State Bool -> MXexpr
(define (tie-note-there->musicxml nt st chord?)
  (match-define (state _ div) st)
  (match nt
    [(data/timed (data/time-period _ d)
                 (tie-note t-start? t-end? n))
     (define duration-str
       (number->string (data/duration-n/divisions d div)))
     (apply note
       `(,@(if chord? `[,(chord)] `[])
         ,(note->musicxml-pitch n)
         ,(duration duration-str)
         ,@(if t-start? `[,(tie #:type "start")] `[])
         ,@(if t-end? `[,(tie #:type "stop")] `[])
         ,@(duration->musicxml-note-type d)
         ;; notations needs to come after everything else so far
         ,(tie-note-there->musicxml-notations nt)))]))

;; tie-note-there->musicxml-notations : TieNoteThere -> MXexpr
(define (tie-note-there->musicxml-notations nt)
  (match nt
    [(data/timed _
                 (tie-note t-start? t-end? n))
     (apply notations
       `(,@(if t-start? `[,(tied #:type "start")] `[])
         ,@(if t-end? `[,(tied #:type "stop")] `[])))]))

;; note->musicxml-pitch : Note -> MXexpr
(define (note->musicxml-pitch n)
  (define alteration (data/note-alteration n))
  (cond
    [(zero? alteration)
     (pitch
      (step (data/note-name-string n))
      (octave (number->string (data/note-octave n))))]
    [else
     (pitch
      (step (data/note-name-string n))
      (alter (number->string alteration))
      (octave (number->string (data/note-octave n))))]))

;; duration->musicxml-note-type : Duration -> [Listof MXexpr]
(define (duration->musicxml-note-type d)
  (cond [(data/duration=? d data/duration-quarter)
         (list (type "quarter"))]
        [(data/duration=? d data/duration-eighth)
         (list (type "eighth"))]
        [(data/duration=? d data/duration-sixteenth)
         (list (type "16th"))]
        [(data/duration=? d data/duration-32nd)
         (list (type "32nd"))]
        [(data/duration=? d data/duration-64th)
         (list (type "64th"))]
        [(data/duration=? d data/duration-128th)
         (list (type "128th"))]
        [(data/duration=? d data/duration-256th)
         (list (type "256th"))]
        [(data/duration=? d data/duration-512th)
         (list (type "512th"))]
        [(data/duration=? d data/duration-1024th)
         (list (type "1024th"))]

        [(data/duration=? d data/duration-half)
         (list (type "half"))]
        [(data/duration=? d data/duration-whole)
         (list (type "whole"))]
        [(data/duration=? d data/duration-double-whole)
         (list (type "breve"))]
        [(data/duration=? d data/duration-quadruple-whole)
         (list (type "long"))]
        [(data/duration=? d data/duration-whole*8)
         (list (type "maxima"))]

        [(data/duration=? d data/duration-dotted-quarter)
         (list (type "quarter") (dot))]
        [(data/duration=? d data/duration-dotted-eighth)
         (list (type "eighth") (dot))]
        [(data/duration=? d data/duration-dotted-half)
         (list (type "half") (dot))]
        [(data/duration=? d data/duration-dotted-whole)
         (list (type "whole") (dot))]
        [else (printf "duration->musicxml-note-type: given: ~v\n" d)
              (list)]))

;; ------------------------------------------------------------------------

(module+ test
  (define SIMPLE-EXAMPLE/MusicXML
    (score->musicxml SIMPLE-EXAMPLE))
  (define CHANGING-TIME-SIG/MusicXML
    (score->musicxml CHANGING-TIME-SIG))

  (check-txexprs-equal?
    SIMPLE-EXAMPLE/MusicXML
    (score-partwise
     #:version "3.0"
     (part-list
      (score-part #:id "P1" (part-name "Music")))
     (part #:id "P1"
       (measure #:number "1"
         (attributes
          (divisions "2"))
         (attributes
          (clef #:sign "G" #:line "2"))
         (attributes
          (key #:fifths "0"))
         (attributes
          (time #:beats "4" #:beat-type "4"))
         (direction #:placement "above"
          (direction-type
           (metronome (beat-unit "quarter") (per-minute "100")))
          (sound #:tempo "100"))
         (note
          (rest)
          (duration "2"))
         (note
          (pitch (step "C") (octave "4"))
          (duration "2")
          (type "quarter")
          (notations))
         (note
          (pitch (step "D") (octave "4"))
          (duration "2")
          (type "quarter")
          (notations))
         (note
          (pitch (step "E") (octave "4"))
          (duration "2")
          (type "quarter")
          (notations))
         (note
          (chord)
          (pitch (step "G") (octave "4"))
          (duration "2")
          (type "quarter")
          (notations)))
       (measure #:number "2"
         (note
          (pitch (step "F") (octave "4"))
          (duration "2")
          (type "quarter")
          (notations))
         (note
          (chord)
          (pitch (step "A") (octave "4"))
          (duration "2")
          (type "quarter")
          (notations))
         (note
          (pitch (step "E") (octave "4"))
          (duration "1")
          (type "eighth")
          (notations))
         (backup
          (duration "1"))
         (note
          (pitch (step "B") (octave "4"))
          (duration "2")
          (type "quarter")
          (notations))
         (backup
          (duration "1"))
         (note
          (pitch (step "D") (octave "4"))
          (duration "1")
          (type "eighth")
          (notations))
         (note
          (pitch (step "E") (octave "4"))
          (duration "4")
          (type "half")
          (notations))
         (note
          (chord)
          (pitch (step "C") (octave "5"))
          (duration "4")
          (type "half")
          (notations))))))

  (check-txexprs-equal?
    CHANGING-TIME-SIG/MusicXML
    (score-partwise
     #:version "3.0"
     (part-list
      (score-part #:id "P1" (part-name "Music")))
     (part #:id "P1"
       (measure #:number "1"
         (attributes (divisions "1"))
         (attributes (clef #:sign "G" #:line "2"))
         (attributes (key #:fifths "0"))
         (attributes (time #:beats "1" #:beat-type "4"))
         (direction #:placement "above"
          (direction-type
           (metronome (beat-unit "quarter") (per-minute "100")))
          (sound #:tempo "100"))
         (note
          (pitch (step "C") (octave "4"))
          (duration "1")
          (type "quarter")
          (notations)))
       (measure #:number "2"
         (attributes (time #:beats "2" #:beat-type "4"))
         (note
          (pitch (step "D") (octave "4"))
          (duration "1")
          (type "quarter")
          (notations))
         (note (rest) (duration "1")))
       (measure #:number "3"
         (note (rest) (duration "1"))
         (note
          (pitch (step "E") (octave "4"))
          (duration "1")
          (tie #:type "start")
          (type "quarter")
          (notations (tied #:type "start"))))
       (measure #:number "4"
         (attributes (time #:beats "3" #:beat-type "4"))
         (note
          (pitch (step "E") (octave "4"))
          (duration "1")
          (tie #:type "stop")
          (type "quarter")
          (notations (tied #:type "stop")))
         (note (rest) (duration "2")))))))

(module+ demo
  (pretty-write SIMPLE-EXAMPLE/MusicXML)

  (define-runtime-path simple-example.xml "simple-example.xml")
  (define-runtime-path changing-time-sig.xml "changing-time-sig.xml")

  (write-musicxml-file simple-example.xml SIMPLE-EXAMPLE/MusicXML
                       #:exists 'replace)
  (write-musicxml-file changing-time-sig.xml CHANGING-TIME-SIG/MusicXML
                       #:exists 'replace)

  (open-musicxml-file/MuseScore-2 simple-example.xml)

  )

;; ------------------------------------------------------------------------

