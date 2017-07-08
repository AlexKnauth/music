#lang agile

(require (submod txexpr safe)
         "musicxml-file.rkt"
         (prefix-in data/
           (combine-in
            "../data/note.rkt"
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

(define (octave . elements)
  (txexpr 'octave '() elements))

(define (duration . elements)
  (txexpr 'duration '() elements))

(define (voice . elements)
  (txexpr 'voice '() elements))

(define (type . elements)
  (txexpr 'type '() elements))

;; ------------------------------------------------------------------------

;; score->musicxml : Score -> MXexpr
(define (score->musicxml s)
  (match s
    [(data/score _ measure-length parts)
     (apply score-partwise
       #:version "3.0"
       (apply part-list
         (for/list ([p (in-list parts)]
                    [i (in-naturals 1)])
           (score-part #:id (part-id i)
             (part-name (data/part-name p)))))
       (for/list ([p (in-list parts)]
                  [i (in-naturals 1)])
         (part->musicxml p i)))]))

(define (part-id i)
  (format "P~a" i))

;; part->musicxml : Part Nat -> MXexpr
(define (part->musicxml p i)
  (match p
    [(data/part _ ms)
     (apply part #:id (part-id i)
       (measures->musicxml-elements ms))]))

;; measures->musicxml-elements : [Listof Measure] -> [Listof MXexpr]
(define (measures->musicxml-elements ms)
  (define div
    (apply data/duration-common-divisions
      (for*/list ([m (in-list ms)]
                  [nst (in-list (data/measure-sorted-notes m))]
                  [nt (in-list (data/notes-there-notes nst))])
        (data/note-there-duration nt))))
  (measures->musicxml-elements/acc ms 0 div '()))

;; measures->musicxml-elements/acc :
;; [Listof Measure] [Listof MXexpr] -> [Listof MXexpr]
(define (measures->musicxml-elements/acc ms n div acc)
  (match ms
    ['() (reverse acc)]
    [(cons fst rst)
     (measures->musicxml-elements/acc rst (add1 n) div
       (cons (measure->musicxml fst n div)
             acc))]))

;; measure->musicxml : Measure Nat PosInt -> MXexpr
(define (measure->musicxml m n div)
  (match m
    [(data/measure sorted-notes)
     (define number-str (number->string (add1 n)))
     (define div-str (number->string div))
     (define pos (data/position n data/duration-zero))
     (cond
       [(zero? n)
        (apply measure #:number number-str
          (attributes
           (divisions div-str)
           (key #:fifths "0")
           (time #:beats "4" #:beat-type "4")
           (clef #:sign "G" #:line "2"))
          (reverse
           (sorted-notes->rev-musicxml-elements sorted-notes
                                                div
                                                pos
                                                '())))]
       [else
        (apply measure #:number number-str
          (reverse
           (sorted-notes->rev-musicxml-elements sorted-notes
                                                div
                                                pos
                                                '())))])]))

;; sorted-notes->rev-musicxml-elements :
;; SortedNotes PosInt Position [Listof MXexpr] -> [Listof MXexpr]
(define (sorted-notes->rev-musicxml-elements sorted-notes div pos acc)
  (match sorted-notes
    ['() acc]
    [(cons fst rst)
     (match-define (data/notes-there note-pos notes) fst)
     (define chords (group-by data/note-there-duration notes data/duration=?))
     (define acc*
       (adjust-position->rev-musicxml-elements pos note-pos div acc))
     (chords->rev-musicxml-elements note-pos chords rst div note-pos
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
  (note (rest) (duration (format "~a" n))))

;; backup-duration->musicxml : Duration PosInt -> MXexpr
(define (backup-duration->musicxml d divisions)
  (define n (data/duration-n/divisions d divisions))
  (backup (duration (format "~a" n))))

;; chords->rev-musicxml-elements :
;;   Position
;;   [Listof [NEListof NoteThere]]
;;   SortedNotes
;;   Position
;;   [Listof MXexpr]
;;   ->
;;   [Listof MXexpr]
(define (chords->rev-musicxml-elements note-pos
                                       chords
                                       sorted-notes
                                       div
                                       pos
                                       acc)
  (match chords
    ['()
     (sorted-notes->rev-musicxml-elements sorted-notes div pos acc)]
    [(cons fst rst)
     (define d (data/note-there-duration (first fst)))
     ;; pos is now note-pos
     (define acc*
       (adjust-position->rev-musicxml-elements pos note-pos div acc))
     (chords->rev-musicxml-elements note-pos rst sorted-notes div
       (data/position+ note-pos d)
       (for/fold ([acc acc*]) ([n (in-list fst)]
                               [i (in-naturals)])
         (cons
          (note-there->musicxml n div (not (zero? i)))
          acc)))]))

;; note-there->musicxml : NoteThere PosInt Bool -> MXexpr
(define (note-there->musicxml nt divisions chord?)
  (match nt
    [(data/note-there _ d n)
     (define duration-str
       (format "~a" (data/duration-n/divisions d divisions)))
     (cond
       [chord?
        (note
         (chord)
         (note->musicxml-pitch n)
         (duration duration-str)
         (duration->musicxml-note-type d))]
       [else
        (note
         (note->musicxml-pitch n)
         (duration duration-str)
         (duration->musicxml-note-type d))])]))

;; note->musicxml-pitch : Note -> MXexpr
(define (note->musicxml-pitch n)
  (pitch
   (step (data/note-name-string n))
   ;; TODO: return the actual octave
   (octave (number->string (data/note-octave n)))))

;; duration->musicxml-note-type : Duration -> MXexpr
(define (duration->musicxml-note-type d)
  (cond [(data/duration=? d data/duration-quarter) (type "quarter")]
        [(data/duration=? d data/duration-eighth) (type "eighth")]
        [(data/duration=? d data/duration-half) (type "half")]
        [else (error 'duration->musicxml-note-type "given: ~v" d)]))

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

