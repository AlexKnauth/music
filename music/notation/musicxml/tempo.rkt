#lang agile

(require musicxml/direction
         racket/format
         (prefix-in data/
           (combine-in
            music/data/time/main)))
(module+ test
  (require rackunit))

;; ---------------------------------------------------------

(provide direction direction-type
         metronome beat-unit beat-unit-dot per-minute
         sound)

;; ---------------------------------------------------------

(provide tempo->direction-musicxml)

;; tempo->direction-musicxml : Tempo -> MXexpr
(define (tempo->direction-musicxml t)
  (match-define (data/tempo bpm b) t)
  (define frac (data/duration-fraction b data/duration-quarter))
  (direction
   '([placement "above"])
   (list
    (direction-type '()
     (list
      (metronome '()
       (append
        (cond
          [(data/duration=? b data/duration-whole)
           (list (beat-unit '() '("whole")))]
          [(data/duration=? b data/duration-half)
           (list (beat-unit '() '("half")))]
          [(data/duration=? b data/duration-quarter)
           (list (beat-unit '() '("quarter")))]
          [(data/duration=? b data/duration-eighth)
           (list (beat-unit '() '("eighth")))]
          [(data/duration=? b data/duration-sixteenth)
           (list (beat-unit '() '("16th")))]
          [(data/duration=? b data/duration-dotted-whole)
           (list (beat-unit '() '("whole")) (beat-unit-dot '() '()))]
          [(data/duration=? b data/duration-dotted-half)
           (list (beat-unit '() '("half")) (beat-unit-dot '() '()))]
          [(data/duration=? b data/duration-dotted-quarter)
           (list (beat-unit '() '("quarter")) (beat-unit-dot '() '()))]
          [(data/duration=? b data/duration-dotted-eighth)
           (list (beat-unit '() '("eighth")) (beat-unit-dot '() '()))]
          [else (error 'tempo "given beat-type: ~v" b)])
        (list
         (per-minute '() (list (number->string bpm))))))))
    (sound `([tempo ,(~r (* frac bpm))]) '()))))

;; ---------------------------------------------------------

(module+ test
  (check-equal? (tempo->direction-musicxml
                  (data/tempo 100 data/duration-quarter))
                (direction '([placement "above"])
                  (list
                   (direction-type '()
                     (list
                      (metronome '()
                        (list (beat-unit '() '("quarter"))
                              (per-minute  '() '("100"))))))
                   (sound '([tempo "100"]) '()))))
  )

