#lang agile

;; The main function of this file is score->musicxml

(require racket/format
         (submod txexpr safe)
         musicxml/attributes
         musicxml/music-data
         "musicxml-file.rkt"
         "metadata.rkt"
         "harmony-element.rkt"
         "clef.rkt"
         "key-signature.rkt"
         "time-signature.rkt"
         "tempo.rkt"
         "pitch.rkt"
         "note.rkt"
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

  (measures->musicxml measures '() #f init-s))

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
        (attributes '()
         (list
          (divisions '() (list div-str))))
        mx-elems)
      next-tie-conts
      st*)]
    [else
     (values
      (apply measure #:number number-str
        mx-elems)
      next-tie-conts
      st*)]))

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

  (define-values [chord other-elements]
    (partition tie-note? elems))

  (define others
    (append-map
     other-element->musicxml
     ;; TODO: What if some other musical element is "tied" over a barline?
     (map tie-info-value other-elements)))

  (define mx-elems
    (chord->musicxml d chord voice (state-divisions group-st)))
  (values
   (st+dur group-st d)
   (append adj others mx-elems)))

;; other-element->musicxml :
;; MusElement -> [Listof MXexpr]
(define (other-element->musicxml e)
  (cond
    [(data/clef? e)
     (list (attributes '() (list (clef->attribute-musicxml e))))]
    [(data/key? e)
     (list (attributes '() (list (key->attribute-musicxml e))))]
    [(data/time-sig? e)
     (list (attributes '() (list (time->attribute-musicxml e))))]
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

;; backup-duration->musicxml : Duration PosInt -> MXexpr
(define (backup-duration->musicxml d divisions)
  (define n (data/duration-n/divisions d divisions))
  (backup '() (list (duration '() (list (number->string n))))))

;; chord->musicxml :
;; Duration [NEListof TieNote] Nat PosInt -> [Listof MXexpr]
;; The notes take up duration d
(define (chord->musicxml d notes voice div)
  (for/list ([nt (in-list notes)]
             [i (in-naturals)])
    (tie-note->musicxml nt d voice div (not (zero? i)))))

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
         (attributes '()
          (list (divisions '() '("2"))))
         (attributes '()
          (list
           (clef '()
                 (list (sign '() '("G"))
                       (line '() '("2"))))))
         (attributes '()
          (list
           (key '() (list (fifths '() '("0"))))))
         (attributes '()
          (list
           (time '()
                 (list (beats '() '("4"))
                       (beat-type '() '("4"))))))
         (direction '([placement "above"])
           (list
            (direction-type '()
              (list
               (metronome '()
                          (list (beat-unit '() '("quarter"))
                                (per-minute  '() '("100"))))))
            (sound '([tempo "100"]) '())))
         (note '()
          (list
           (rest '() '())
           (duration '() '("2"))
           (voice '() '("1"))))
         (note '()
          (list
           (pitch '() (list (step '() '("C")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (pitch '() (list (step '() '("D")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (pitch '() (list (step '() '("E")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (chord '() '())
           (pitch '() (list (step '() '("G")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '()))))
       (measure #:number "2"
         (note '()
          (list
           (pitch '() (list (step '() '("F")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (chord '() '())
           (pitch '() (list (step '() '("A")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (pitch '() (list (step '() '("B")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (pitch '() (list (step '() '("E")) (octave '() '("4"))))
           (duration '() '("4"))
           (voice '() '("1"))
           (type '() '("half"))
           (notations '() '())))
         (note '()
          (list
           (chord '() '())
           (pitch '() (list (step '() '("C")) (octave '() '("5"))))
           (duration '() '("4"))
           (voice '() '("1"))
           (type '() '("half"))
           (notations '() '())))
         (backup '()
          (list (duration '() '("6"))))
         (note '()
          (list
           (pitch '() (list (step '() '("E")) (octave '() '("4"))))
           (duration '() '("1"))
           (voice '() '("2"))
           (type '() '("eighth"))
           (notations '() '())))
         (note '()
          (list
           (pitch '() (list (step '() '("D")) (octave '() '("4"))))
           (duration '() '("1"))
           (voice '() '("2"))
           (type '() '("eighth"))
           (notations '() '())))
         (note '()
          (list
           (rest '() '())
           (duration '() '("4"))
           (voice '() '("2"))))))))

  (check-txexprs-equal?
    CHANGING-TIME-SIG/MusicXML
    (score-partwise
     #:version "3.0"
     (part-list
      (score-part #:id "P1" (part-name "Music")))
     (part #:id "P1"
       (measure #:number "1"
         (attributes '() (list (divisions '() '("1"))))
         (attributes '()
          (list (clef '()
                      (list (sign '() '("G"))
                            (line '() '("2"))))))
         (attributes '()
          (list (key '() (list (fifths '() '("0"))))))
         (attributes '()
          (list (time '()
                      (list (beats '() '("1"))
                            (beat-type '() '("4"))))))
         (direction '([placement "above"])
           (list
            (direction-type '()
              (list
               (metronome '()
                          (list (beat-unit '() '("quarter"))
                                (per-minute '() '("100"))))))
            (sound '([tempo "100"]) '())))
         (note '()
          (list
           (pitch '() (list (step '() '("C")) (octave '() '("4"))))
           (duration '() '("1"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '()))))
       (measure #:number "2"
         (attributes '()
          (list (time '()
                      (list (beats '() '("2"))
                            (beat-type '() '("4"))))))
         (note '()
          (list
           (pitch '() (list (step '() '("D")) (octave '() '("4"))))
           (duration '() '("1"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (rest '() '())
           (duration '() '("1"))
           (voice '() '("1")))))
       (measure #:number "3"
         (note '()
          (list
           (rest '() '())
           (duration '() '("1"))
           (voice '() '("1"))))
         (note '()
          (list
           (pitch '() (list (step '() '("E")) (octave '() '("4"))))
           (duration '() '("1"))
           (tie '([type "start"]) '())
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() (list (tied '([type "start"]) '()))))))
       (measure #:number "4"
         (attributes '()
          (list (time '()
                      (list (beats '() '("3"))
                            (beat-type '() '("4"))))))
         (note '()
          (list
           (pitch '() (list (step '() '("E")) (octave '() '("4"))))
           (duration '() '("1"))
           (tie '([type "stop"]) '())
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() (list (tied '([type "stop"]) '())))))
         (note '()
          (list
           (rest '() '())
           (duration '() '("2"))
           (voice '() '("1")))))))))

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

