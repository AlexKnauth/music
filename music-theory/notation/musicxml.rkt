#lang agile

;; The main function of this file is score->musicxml

(require racket/format
         (submod txexpr safe)
         "musicxml-file.rkt"
         (prefix-in data/
           (combine-in
            "../data/note.rkt"
            "../data/note-held.rkt"
            "../data/note-there.rkt"
            "../data/score.rkt")))
(module+ test
  (require rackunit
           racket/runtime-path
           racket/pretty
           (submod "../data/score.rkt" example)))

;; A MXexpr is a TXexpr in MusicXML format

;; ------------------------------------------------------------------------

;; %partwise

(define (score-partwise #:version version-str . elements)
  (txexpr 'score-partwise `([version ,version-str]) elements))

;; ------------------------------------------------------------------------

;; %score-header

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

(define (clef #:sign sign-str #:line line-str)
  (txexpr 'clef '()
    (list
     (txexpr 'sign '() (list sign-str))
     (txexpr 'line '() (list line-str)))))

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

(define (voice . elements)
  (txexpr 'voice '() elements))

(define (type . elements)
  (txexpr 'type '() elements))

(define (dot)
  (txexpr 'dot '() '()))

;; ------------------------------------------------------------------------

(provide score->musicxml)

;; score->musicxml : Score -> MXexpr
(define (score->musicxml s)
  (match s
    [(data/score key tempo measure-length parts)
     (apply score-partwise
       #:version "3.0"
       (apply part-list
         (for/list ([p (in-list parts)]
                    [i (in-naturals 1)])
           (score-part #:id (part-id i)
             (part-name (data/part-name p)))))
       (for/list ([p (in-list parts)]
                  [i (in-naturals 1)])
         (part->musicxml p i key tempo measure-length)))]))

(define (part-id i)
  (format "P~a" i))

;; part->musicxml : Part Nat Key Tempo Duration -> MXexpr
(define (part->musicxml p i key tempo measure-length)
  (match p
    [(data/part _ sorted-notes)
     (apply part #:id (part-id i)
       (measures->musicxml-elements sorted-notes key tempo measure-length))]))

;; measures->musicxml-elements :
;; SortedNotes Key Tempo Duration -> [Listof MXexpr]
(define (measures->musicxml-elements sorted-notes k t ml)
  (define div
    (apply data/duration-common-divisions
      (for*/list ([nst (in-list sorted-notes)]
                  [nt (in-list (data/notes-there-notes nst))])
        (data/note-there-duration nt))))
  (define groups
    (group-by data/notes-there-measure-number
              sorted-notes))
  (define measures
    (let loop ([acc '()] [i 0] [groups groups])
      (match groups
        ['() (reverse acc)]
        [(cons group groups)
         (cond
           [(= i (data/notes-there-measure-number (first group)))
            (loop (cons group acc) (add1 i) groups)]
           [else
            (loop (cons '() acc) (add1 i) (cons group groups))])])))
  (measures->musicxml-elements/acc measures 0 k t ml div '()))

;; measures->musicxml-elements/acc :
;; [Listof SortedNotes] Nat Key Tempo Duration PosInt [Listof MXexpr]
;; -> [Listof MXexpr]
(define (measures->musicxml-elements/acc ms n k t ml div acc)
  (match ms
    ['() (reverse acc)]
    [(cons fst rst)
     (measures->musicxml-elements/acc rst (add1 n) k t ml div
       (cons (measure->musicxml fst n k t ml div)
             acc))]))

;; measure->musicxml : SortedNotes Nat Key Tempo Duration PosInt -> MXexpr
(define (measure->musicxml sorted-notes n k t ml div)
  (define number-str (number->string (add1 n)))
  (define div-str (number->string div))
  (define pos (data/position n data/duration-zero))
  (cond
    [(zero? n)
     (apply measure #:number number-str
       (attributes
        (divisions div-str)
        (key #:fifths (number->string (data/key-fifths k)))
        (time->attribute-musicxml t ml)
        (clef #:sign "G" #:line "2"))
       (time->direction-musicxml t ml)
       (reverse
        (sorted-notes->rev-musicxml-elements sorted-notes
                                             ml
                                             div
                                             pos
                                             '())))]
    [else
     (apply measure #:number number-str
       (reverse
        (sorted-notes->rev-musicxml-elements sorted-notes
                                             ml
                                             div
                                             pos
                                             '())))]))

;; time->attribute-musicxml : Tempo Duration -> MXexpr
(define (time->attribute-musicxml t ml)
  (define-values [beats beat-type]
    (data/duration-divide ml (data/tempo-beat-length t)))
  (time #:beats (number->string beats)
        #:beat-type
        (cond
          [(data/duration=? beat-type data/duration-whole) "1"]
          [(data/duration=? beat-type data/duration-half) "2"]
          [(data/duration=? beat-type data/duration-quarter) "4"]
          [(data/duration=? beat-type data/duration-eighth) "8"]
          [(data/duration=? beat-type data/duration-sixteenth) "16"]
          [else (error 'type->musicxml "given beat-type: ~v" beat-type)])))

;; time->direction-musicxml : Tempo Duration -> MXexpr
(define (time->direction-musicxml t ml)
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

;; sorted-notes->rev-musicxml-elements :
;; SortedNotes Duration PosInt Position [Listof MXexpr] -> [Listof MXexpr]
(define (sorted-notes->rev-musicxml-elements sorted-notes ml div pos acc)
  (match sorted-notes
    ['()
     (define measure-end (data/position (data/position-measure-number pos) ml))
     (adjust-position->rev-musicxml-elements pos measure-end div acc)]
    [(cons fst rst)
     (match-define (data/notes-there note-pos notes) fst)
     (define chords (group-by data/note-there-duration notes data/duration=?))
     (define acc*
       (adjust-position->rev-musicxml-elements pos note-pos div acc))
     (chords->rev-musicxml-elements note-pos chords rst ml div note-pos
       acc*)]))

;; adjust-position->rev-musicxml-elements :
;; Position Position PosInt [Listof MXexpr] -> [Listof MXexpr]
(define (adjust-position->rev-musicxml-elements pos note-pos div acc)
  (cond [(data/position=? pos note-pos) acc]
        [(data/position<? pos note-pos)
         (cons (rest-duration->musicxml (data/position∆ pos note-pos) div)
               acc)]
        [else
         (cons (backup-duration->musicxml (data/position∆ note-pos pos) div)
               acc)]))

;; rest-duration->musicxml : Duration PosInt -> MXexpr
(define (rest-duration->musicxml d divisions)
  (define n (data/duration-n/divisions d divisions))
  (note (rest) (duration (number->string n))))

;; backup-duration->musicxml : Duration PosInt -> MXexpr
(define (backup-duration->musicxml d divisions)
  (define n (data/duration-n/divisions d divisions))
  (backup (duration (number->string n))))

;; chords->rev-musicxml-elements :
;;   Position
;;   [Listof [NEListof NoteThere]]
;;   SortedNotes
;;   Duration
;;   PosInt
;;   Position
;;   [Listof MXexpr]
;;   ->
;;   [Listof MXexpr]
(define (chords->rev-musicxml-elements note-pos
                                       chords
                                       sorted-notes
                                       ml
                                       div
                                       pos
                                       acc)
  (match chords
    ['()
     (sorted-notes->rev-musicxml-elements sorted-notes ml div pos acc)]
    [(cons fst rst)
     (define d (data/note-there-duration (first fst)))
     ;; pos is now note-pos
     (define acc*
       (adjust-position->rev-musicxml-elements pos note-pos div acc))
     (chords->rev-musicxml-elements note-pos rst sorted-notes ml div
       (data/position+ note-pos d)
       (for/fold ([acc acc*]) ([n (in-list fst)]
                               [i (in-naturals)])
         (cons
          (note-there->musicxml n div (not (zero? i)))
          acc)))]))

;; note-there->musicxml : NoteThere PosInt Bool -> MXexpr
(define (note-there->musicxml nt divisions chord?)
  (match nt
    [(data/note-there _ (data/note-held n d))
     (define duration-str
       (number->string (data/duration-n/divisions d divisions)))
     (cond
       [chord?
        (apply note
          (list*
           (chord)
           (note->musicxml-pitch n)
           (duration duration-str)
           (duration->musicxml-note-type d)))]
       [else
        (apply note
          (list*
           (note->musicxml-pitch n)
           (duration duration-str)
           (duration->musicxml-note-type d)))])]))

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
        [(data/duration=? d data/duration-half)
         (list (type "half"))]
        [(data/duration=? d data/duration-whole)
         (list (type "whole"))]
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

  (pretty-write SIMPLE-EXAMPLE/MusicXML)
  (check-txexprs-equal?
    SIMPLE-EXAMPLE/MusicXML
    (score-partwise
     #:version "3.0"
     (part-list
      (score-part #:id "P1" (part-name "Music")))
     (part #:id "P1"
       (measure #:number "1"
         (attributes
          (divisions "2")
          (key #:fifths "0")
          (time #:beats "4" #:beat-type "4")
          (clef #:sign "G" #:line "2"))
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
          (type "quarter"))
         (note
          (pitch (step "D") (octave "4"))
          (duration "2")
          (type "quarter"))
         (note
          (pitch (step "E") (octave "4"))
          (duration "2")
          (type "quarter"))
         (note
          (chord)
          (pitch (step "G") (octave "4"))
          (duration "2")
          (type "quarter")))
       (measure #:number "2"
         (note
          (pitch (step "F") (octave "4"))
          (duration "2")
          (type "quarter"))
         (note
          (chord)
          (pitch (step "A") (octave "4"))
          (duration "2")
          (type "quarter"))
         (note
          (pitch (step "E") (octave "4"))
          (duration "1")
          (type "eighth"))
         (backup
          (duration "1"))
         (note
          (pitch (step "B") (octave "4"))
          (duration "2")
          (type "quarter"))
         (backup
          (duration "1"))
         (note
          (pitch (step "D") (octave "4"))
          (duration "1")
          (type "eighth"))
         (note
          (pitch (step "E") (octave "4"))
          (duration "4")
          (type "half"))
         (note
          (chord)
          (pitch (step "C") (octave "5"))
          (duration "4")
          (type "half"))))))

  (define-runtime-path simple-example.xml "simple-example.xml")
  
  (write-musicxml-file simple-example.xml SIMPLE-EXAMPLE/MusicXML
                       #:exists 'replace)

  (open-musicxml-file/MuseScore-2 simple-example.xml)

  )

;; ------------------------------------------------------------------------

