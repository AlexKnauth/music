#lang agile

;; The main function of this file is score->musicxml

(require racket/format
         fancy-app
         (submod txexpr safe)
         "musicxml-file.rkt"
         "metadata.rkt"
         "harmony-element.rkt"
         "clef.rkt"
         "voice-assign.rkt"
         (prefix-in data/
           (combine-in
            music/data/time/main
            music/data/time/measures
            music/data/note/main
            music/data/score/main)))
(module+ test
  (provide SIMPLE-EXAMPLE/MusicXML CHANGING-TIME-SIG/MusicXML)
  (require rackunit
           (submod music/data/score/score example)))
(module+ demo
  (require racket/runtime-path
           racket/pretty
           (submod music/data/score/score example)
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

(define (lyric #:number number-str . elements)
  (txexpr 'lyric `([number ,number-str]) elements))

(define (syllabic . elements)
  (txexpr 'syllabic '() elements))

(define (text . elements)
  (txexpr 'text '() elements))

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

;; A [TieInfo X] is a (tie-note Bool Bool X)
(struct tie-info [start? end? value])

;; A TieElem is a [TieInfo MusElement]

;; A TieNote is a [TieInfo Note]
(define (tie-note? v)
  (and (tie-info? v) (data/note? (tie-info-value v))))

;; A TieLyric is a [TieInfo Lyric]
(define (tie-lyric? v)
  (and (tie-info? v) (data/lyric? (tie-info-value v))))

;; tie-single,
;; tie-start,
;; tie-end,
;; tie-mid : X -> [TieInfo X]
(define (tie-single n) (tie-info #f #f n))
(define (tie-start n)  (tie-info #t #f n))
(define (tie-end n)    (tie-info #f #t n))
(define (tie-mid n)    (tie-info #t #t n))

;; A TieNoteThere is a [Timed TieNote]
(define (tie-note-there? v)
  (and (data/timed? v)
       (tie-note? (data/timed-value v))))

;; tie-there-single,
;; tie-there-start,
;; tie-there-end,
;; tie-there-mid : [Timed X] -> [Timed [TieInfo X]]
(define (tie-there-single nt) (data/timed-map nt tie-single))
(define (tie-there-start nt)  (data/timed-map nt tie-start))
(define (tie-there-end nt)    (data/timed-map nt tie-end))
(define (tie-there-mid nt)    (data/timed-map nt tie-mid))

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
       #:version "4.0"
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
  (define parts-sorted-notes (map data/part-sorted-elements parts))
  (define parts-measures (map data/group-measures parts-sorted-notes))
  (define ns (map length parts-measures))
  (define n (apply max 0 ns))
  (parts-musicxml-pad-end-measures
   (for/list ([p (in-list parts-measures)]
              [i (in-naturals 1)])
     (part-measures->musicxml (pad-end-measures p n) i))))

;; pad-end-measures : [Listof Measure] Nat -> [Listof Measure]
(define (pad-end-measures measures n)
  (define n0 (length measures))
  (define ts (data/measure-time-sig (last measures)))
  (append measures
          (for/list ([i (in-range n0 n)])
            (data/measure ts '()))))

;; part-measures->musicxml : [Listof Measure] Nat -> MXexpr
(define (part-measures->musicxml measures i)
  (apply part #:id (part-id i)
    (measures->musicxml-elements measures)))

;; measures->musicxml-elements :
;; [Listof Measure] -> [Listof MXexpr]
(define (measures->musicxml-elements measures)
  (define div
    (apply data/duration-common-divisions
      (for*/list ([measure (in-list measures)]
                  [nt (in-list (data/measure-elements measure))]
                  #:when (data/note-there? nt))
        (data/note-there-duration nt))))

  (define init-s
    (state (data/position 0 data/duration-zero) div))

  (measures->musicxml measures '() #f init-s))

;; parts-musicxml-pad-end-measures : [Listof MXexpr] -> [Listof MXexpr]
(define (parts-musicxml-pad-end-measures parts-musicxml)
  (define ns (map part-musicxml-max-measure parts-musicxml))
  (define n (apply max 0 ns))
  (map (part-musicxml-pad-end-measures n) parts-musicxml ns))

;; part-musicxml-max-measure : MXexpr -> Natural
(define (part-musicxml-max-measure part-musicxml)
  (define ms
    (or (findf*-txexpr part-musicxml
                       (λ (x) (and (txexpr? x)
                                   (equal? (get-tag x) 'measure))))
        '()))
  (apply max 0 (map (compose string->number (attr-ref _ 'number)) ms)))

;; part-musicxml-pad-end-measures : Natural -> [MXexpr Natural -> MXexpr]
(define ((part-musicxml-pad-end-measures n) part-musicxml n0)
  (append part-musicxml
          (for/list ([i (in-inclusive-range (add1 n0) n)])
            (measure #:number (number->string i)))))

;; ------------------------------------------------------------------------

;; measures->musicxml :
;; [Listof Measure] [Listof TieCont] [Maybe TimeSig] State -> [Listof MXexpr]
(define (measures->musicxml ms ties ts st)
  (match ms
    ['()
     (cond
       [(empty? ties) '()]
       [else
        (define-values [m ties* st*]
          (measure->musicxml (data/measure ts '()) ties st))
        (cons
         m
         (measures->musicxml '() ties* ts (st/measure-boundary st* ts)))])]
    [(cons fst rst)
     (define ts (data/measure-time-sig fst))
     (define-values [m ties* st*]
       (measure->musicxml fst ties st))
     (cons
      m
      (measures->musicxml rst ties* ts (st/measure-boundary st* ts)))]))

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
     (map tie-there-single single-notes)
     (map tie-there-start tie-starts)
     (map tie-there-mid old-tie-mids)
     (map tie-there-end old-tie-ends)))

  (define next-tie-conts
    (append old-tie-conts new-tie-conts))

  ;; voice-order :
  ;; [Listof [Timed [NEListof TieElem]]] -> [Listof [Timed [NEListof TieElem]]]
  (define (voice-order ttes)
    (define-values [notes elems]
      (partition (λ (x) (ormap tie-note? (data/timed-value x)))
                 ttes))
    (append
     elems
     (sort notes data/note-midi>?
           #:key (compose (λ (x) (argmax data/note-midi-number x))
                          (λ (x) (filter data/note? x))
                          (λ (x) (map tie-info-value x))
                          data/timed-value))))

  ;; voiced-groups : [Listof [Voiced [Listof [Timed [NEListof TieElem]]]]]
  (define voiced-groups
    (assign-voices (data/group-by-time-period tie-notes) voice-order))

  (define number-str (number->string (add1 n)))
  (define div-str (number->string div))

  (define-values [st* mx-elems]
    (voices->musicxml voiced-groups st meas-end))

  (cond
    [(zero? n)
     (values
      (apply measure #:number number-str
        (attributes
         (divisions div-str))
        mx-elems)
      next-tie-conts
      st*)]
    [else
     (values
      (apply measure #:number number-str
        mx-elems)
      next-tie-conts
      st*)]))

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

;; voices->musicxml :
;; [Listof [Voiced [Listof [Timed [NEListof TieElem]]]] State Position
;; ->
;; [Listof MXexpr]
(define (voices->musicxml voices st meas-end)
  (match voices
    ['()
     (values st '())]
    [(cons fst rst)
     (define-values [st* fst-elems]
       (voice->musicxml fst st meas-end))
     (define-values [st** rst-elems]
       (voices->musicxml rst st* meas-end))
     (values
      st**
      (append fst-elems rst-elems))]))

;; voice->musicxml :
;; [Voiced [Listof [Timed [NEListof TieElem]]] State -> [Listof MXexpr]
(define (voice->musicxml voice st meas-end)
  (match-define (voiced vc groups) voice)
  (define-values [st* vc-elems]
    (tp-groups->musicxml groups vc st))
  (define-values [st** meas-end-adj]
    (adjust-position->musicxml st* meas-end vc))
  (values
   st**
   (append vc-elems meas-end-adj)))

;; tp-groups->musicxml :
;; [Listof [Timed [NEListof TieElem]]] Nat State -> [Listof MXexpr]
(define (tp-groups->musicxml groups vc st)
  (match groups
    ['()
     (values st '())]
    [(cons fst rst)
     (define-values [st* fst-elems]
       (tp-group->musicxml fst vc st))
     (define-values [st** rst-elems]
       (tp-groups->musicxml rst vc st*))
     (values
      st**
      (append fst-elems rst-elems))]))

;; tp-group->musicxml :
;; [Timed [NEListof TieElem]] Nat State -> [Listof MXexpr]
;; the state st may or may not line up with the start of the group,
;; adjust using adjust-position->musicxml first
(define (tp-group->musicxml group voice st)
  (match-define (data/timed tp elems) group)
  (match-define (data/time-period group-pos d) tp)

  ;; make adjustments so that group-pos is the new state pos
  (define-values [group-st adj]
    (adjust-position->musicxml st group-pos voice))

  (define-values [chord other-elements1]
    (partition tie-note? elems))

  (define-values [lyrics1 other-elements2]
    (partition tie-lyric? other-elements1))

  ;; TODO: better handle tied lyrics
  (define lyrics2 (map tie-info-value lyrics1))

  (define others
    (append-map
     other-element->musicxml
     ;; TODO: What if some other musical element is "tied" over a barline?
     (map tie-info-value other-elements2)))

  (define mx-elems
    (chord->musicxml d chord voice lyrics2 (state-divisions group-st)))
  (values
   (st+dur group-st d)
   (append adj others mx-elems)))

;; other-element->musicxml :
;; MusElement -> [Listof MXexpr]
(define (other-element->musicxml e)
  (cond
    [(data/clef? e)
     (list (attributes (clef->attribute-musicxml e)))]
    [(data/key? e)
     (list (attributes (key->attribute-musicxml e)))]
    [(data/time-sig? e)
     (list (attributes (time->attribute-musicxml e)))]
    [(data/tempo? e)
     (list (tempo->direction-musicxml e))]
    [(data/harmony-element? e)
     (list (harmony-element->musicxml e))]
    [else
     (error 'other-elements->musicxml "unrecognized element: ~v" e)]))

;; adjust-position->musicxml :
;; State Position Nat -> (values State [Listof MXexpr])
(define (adjust-position->musicxml st note-pos vc)
  (match-define (state pos div) st)
  (cond
    [(data/position=? pos note-pos)  (values st '())]
    [(data/position<? pos note-pos)
     (values
      (state note-pos div)
      (list (rest-duration->musicxml (data/position∆ pos note-pos) vc div)))]
    [else
     (values
      (state note-pos div)
      (list (backup-duration->musicxml (data/position∆ note-pos pos) div)))]))

;; rest-duration->musicxml : Duration Nat PosInt -> MXexpr
(define (rest-duration->musicxml d vc divisions)
  (define n (data/duration-n/divisions d divisions))
  (define vc-str (number->string (add1 vc)))
  (note (rest) (duration (number->string n)) (voice vc-str)))

;; backup-duration->musicxml : Duration PosInt -> MXexpr
(define (backup-duration->musicxml d divisions)
  (define n (data/duration-n/divisions d divisions))
  (backup (duration (number->string n))))

;; chord->musicxml :
;; Duration [NEListof TieNote] Nat [Listof Lyric] PosInt -> [Listof MXexpr]
;; The notes take up duration d
(define (chord->musicxml d notes voice lyrics div)
  (for/list ([nt (in-list notes)]
             [i (in-naturals)])
    (tie-note->musicxml nt d voice lyrics div (not (zero? i)))))

;; tie-note->musicxml : TieNote Duration Nat [Listof Lyric] PosInt Bool -> MXexpr
;; The note takes up duration d
(define (tie-note->musicxml nt d vc lyrics div chord?)
  (match nt
    [(tie-info t-start? t-end? n)
     (define duration-str
       (number->string (data/duration-n/divisions d div)))
     (define voice-str
       (number->string (add1 vc)))
     (apply note
       `(,@(if chord? `[,(chord)] `[])
         ,(note->musicxml-pitch n)
         ,(duration duration-str)
         ,@(if t-start? `[,(tie #:type "start")] `[])
         ,@(if t-end? `[,(tie #:type "stop")] `[])
         ,(voice voice-str)
         ,@(duration->musicxml-note-type d)
         ;; notations needs to come after everything else so far
         ,@(tie-note->musicxml-notations nt)
         ,@(if (not chord?) (map lyric->musicxml-lyric lyrics) `[])))]))

;; tie-note-there->musicxml-notations : TieNote -> [Listof MXexpr]
(define (tie-note->musicxml-notations nt)
  (match nt
    [(tie-info #false #false _)
     (list)]
    [(tie-info t-start? t-end? n)
     (list
      (apply notations
        `(,@(if t-start? `[,(tied #:type "start")] `[])
          ,@(if t-end? `[,(tied #:type "stop")] `[]))))]))

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

;; lyric->musicxml-lyric : Lyric -> MXexpr
(define (lyric->musicxml-lyric l)
  (match l
    [(data/lyric n s t)
     (lyric #:number n (syllabic (symbol->string s)) (text t))]))

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
     #:version "4.0"
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
          (duration "2")
          (voice "1"))
         (note
          (pitch (step "C") (octave "4"))
          (duration "2")
          (voice "1")
          (type "quarter"))
         (note
          (pitch (step "D") (octave "4"))
          (duration "2")
          (voice "1")
          (type "quarter"))
         (note
          (pitch (step "E") (octave "4"))
          (duration "2")
          (voice "1")
          (type "quarter"))
         (note
          (chord)
          (pitch (step "G") (octave "4"))
          (duration "2")
          (voice "1")
          (type "quarter")))
       (measure #:number "2"
         (note
          (pitch (step "F") (octave "4"))
          (duration "2")
          (voice "1")
          (type "quarter"))
         (note
          (chord)
          (pitch (step "A") (octave "4"))
          (duration "2")
          (voice "1")
          (type "quarter"))
         (note
          (pitch (step "B") (octave "4"))
          (duration "2")
          (voice "1")
          (type "quarter"))
         (note
          (pitch (step "E") (octave "4"))
          (duration "4")
          (voice "1")
          (type "half"))
         (note
          (chord)
          (pitch (step "C") (octave "5"))
          (duration "4")
          (voice "1")
          (type "half"))
         (backup
          (duration "6"))
         (note
          (pitch (step "E") (octave "4"))
          (duration "1")
          (voice "2")
          (type "eighth"))
         (note
          (pitch (step "D") (octave "4"))
          (duration "1")
          (voice "2")
          (type "eighth"))
         (note
          (rest)
          (duration "4")
          (voice "2"))))))

  (check-txexprs-equal?
    CHANGING-TIME-SIG/MusicXML
    (score-partwise
     #:version "4.0"
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
          (voice "1")
          (type "quarter")))
       (measure #:number "2"
         (attributes (time #:beats "2" #:beat-type "4"))
         (note
          (pitch (step "D") (octave "4"))
          (duration "1")
          (voice "1")
          (type "quarter"))
         (note (rest) (duration "1") (voice "1")))
       (measure #:number "3"
         (note (rest) (duration "1") (voice "1"))
         (note
          (pitch (step "E") (octave "4"))
          (duration "1")
          (tie #:type "start")
          (voice "1")
          (type "quarter")
          (notations (tied #:type "start"))))
       (measure #:number "4"
         (attributes (time #:beats "3" #:beat-type "4"))
         (note
          (pitch (step "E") (octave "4"))
          (duration "1")
          (tie #:type "stop")
          (voice "1")
          (type "quarter")
          (notations (tied #:type "stop")))
         (note (rest) (duration "2") (voice "1")))))))

(module+ demo
  (pretty-write SIMPLE-EXAMPLE/MusicXML)

  (define-runtime-path simple-example.xml "simple-example.xml")
  (define-runtime-path changing-time-sig.xml "changing-time-sig.xml")

  (write-musicxml-file simple-example.xml SIMPLE-EXAMPLE/MusicXML
                       #:exists 'replace
                       #:indentation 'peek)
  (write-musicxml-file changing-time-sig.xml CHANGING-TIME-SIG/MusicXML
                       #:exists 'replace
                       #:indentation 'peek)

  (open-musicxml-file/MuseScore-4 simple-example.xml)

  )

;; ------------------------------------------------------------------------

