#lang agile

(require musicxml/note
         musicxml/duration
         musicxml/voice
         "pitch.rkt"
         (prefix-in data/
           (combine-in
            music/data/time/main
            music/data/note/main)))
(module+ test
  (require rackunit))

;; ---------------------------------------------------------

(provide note
         chord
         rest
         duration
         tie
         voice
         type
         notations
         tied)

;; ---------------------------------------------------------

(provide tie-info tie-info? tie-info-value
         tie-note?
         tie-note-there?
         tie-single
         tie-start
         tie-end
         tie-mid)

;; A [TieInfo X] is a (tie-note Bool Bool X)
(struct tie-info [start? end? value])

;; A TieElem is a [TieInfo MusElement]

;; A TieNote is a [TieInfo Note]
(define (tie-note? v)
  (and (tie-info? v) (data/note? (tie-info-value v))))

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

;; ---------------------------------------------------------

(provide tie-note->musicxml)

;; Does not include chord or voice properties. Those will be
;; added by "score.rkt".

;; tie-note->musicxml : TieNote Duration PosInt -> MXexpr
;; The note takes up duration d
(define (tie-note->musicxml nt d div)
  (match nt
    [(tie-info t-start? t-end? n)
     (define duration-str
       (number->string (data/duration-n/divisions d div)))
     (note
      '()
      `(,(note->musicxml-pitch n)
        ,(duration '() (list duration-str))
        ,@(if t-start? `[,(tie '([type "start"]) '())] `[])
        ,@(if t-end? `[,(tie '([type "stop"]) '())] `[])
        ,@(duration->musicxml-note-type d)
        ;; notations needs to come after everything else so far
        ,(tie-note->musicxml-notations nt)))]))

;; tie-note-there->musicxml-notations : TieNote -> MXexpr
(define (tie-note->musicxml-notations nt)
  (match nt
    [(tie-info t-start? t-end? n)
     (notations
      '()
      `(,@(if t-start? `[,(tied '([type "start"]) '())] `[])
        ,@(if t-end? `[,(tied '([type "stop"]) '())] `[])))]))

;; duration->musicxml-note-type : Duration -> [Listof MXexpr]
(define (duration->musicxml-note-type d)
  (cond [(data/duration=? d data/duration-quarter)
         (list (type '() '("quarter")))]
        [(data/duration=? d data/duration-eighth)
         (list (type'() '("eighth")))]
        [(data/duration=? d data/duration-sixteenth)
         (list (type '() '("16th")))]
        [(data/duration=? d data/duration-32nd)
         (list (type '() '("32nd")))]
        [(data/duration=? d data/duration-64th)
         (list (type '() '("64th")))]
        [(data/duration=? d data/duration-128th)
         (list (type '() '("128th")))]
        [(data/duration=? d data/duration-256th)
         (list (type '() '("256th")))]
        [(data/duration=? d data/duration-512th)
         (list (type '() '("512th")))]
        [(data/duration=? d data/duration-1024th)
         (list (type '() '("1024th")))]

        [(data/duration=? d data/duration-half)
         (list (type '() '("half")))]
        [(data/duration=? d data/duration-whole)
         (list (type '() '("whole")))]
        [(data/duration=? d data/duration-double-whole)
         (list (type '() '("breve")))]
        [(data/duration=? d data/duration-quadruple-whole)
         (list (type '() '("long")))]
        [(data/duration=? d data/duration-whole*8)
         (list (type '() '("maxima")))]

        [(data/duration=? d data/duration-dotted-quarter)
         (list (type '() '("quarter")) (dot '() '()))]
        [(data/duration=? d data/duration-dotted-eighth)
         (list (type '() '("eighth")) (dot '() '()))]
        [(data/duration=? d data/duration-dotted-half)
         (list (type '() '("half")) (dot '() '()))]
        [(data/duration=? d data/duration-dotted-whole)
         (list (type '() '("whole")) (dot '() '()))]
        [else (printf "duration->musicxml-note-type: given: ~v\n" d)
              (list)]))

;; ---------------------------------------------------------

(provide rest-duration->musicxml)

;; Does not include chord or voice properties. Those will be
;; added by "score.rkt".

;; rest-duration->musicxml : Duration PosInt -> MXexpr
(define (rest-duration->musicxml d divisions)
  (define n (data/duration-n/divisions d divisions))
  (note
   '()
   (list
    (rest '() '())
    (duration '() (list (number->string n))))))

;; ---------------------------------------------------------

