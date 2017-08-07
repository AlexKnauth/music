#lang agile

(require 2htdp/image
         racket/bool
         music-theory/data/note/note
         music-theory/data/instrument/main)
(module+ demo
  (require (submod music-theory/data/note/note example)
           music-theory/data/chord/chord
           (submod music-theory/data/instrument/fingering example)
           (submod music-theory/data/instrument/chord-fingering example)))

(define (truth? b) (if b #true #false))

;; ------------------------------------------------------------------------

(define STRING-WIDTH 20)
(define FRET-HEIGHT 22)

(define CIRCLE-RADIUS 9)

(define MUTE-FONT-SIZE 18)

(define NUT-BOX
  (scene+line
   (rectangle STRING-WIDTH FRET-HEIGHT "solid" "white")
   0 FRET-HEIGHT
   STRING-WIDTH FRET-HEIGHT
   (make-pen "black" 5 "solid" "round" "round")))

(define FRET-BOX
  (scene+line
   (scene+line
    (rectangle STRING-WIDTH FRET-HEIGHT "solid" "white")
    (* 1/2 STRING-WIDTH) 0
    (* 1/2 STRING-WIDTH) FRET-HEIGHT
    (make-pen "black" 1 "solid" "round" "round"))
   0 FRET-HEIGHT
   STRING-WIDTH FRET-HEIGHT
   (make-pen "black" 2 "solid" "round" "round")))

(define OPEN-CIRCLE
  (overlay
   (circle CIRCLE-RADIUS "outline" "black")
   (circle CIRCLE-RADIUS "solid" "white")))

(define CLOSED-CIRCLE
  (circle CIRCLE-RADIUS "solid" "black"))

(define MUTE-STRING
  (text "X" MUTE-FONT-SIZE "black"))

(define FINGER-NUM-FONT-SIZE 10)
(define FINGER-NUM-COLOR "white")

;; ------------------------------------------------------------------------

(provide guitar-chord-chart)

;; guitar-chord-chart : ChordLayout -> Image
(define (guitar-chord-chart chord-layout)
  (define n
    (add1 (apply max 4 (map ivl-midi∆ (filter truth? chord-layout)))))
  (apply beside
    (for/list ([ivl (in-list chord-layout)])
      (guitar-chord-chart/string ivl n))))

;; guitar-chord-chart/fingering : ChordFingering -> Image
(define (guitar-chord-chart/fingering chord-fingering)
  (define n
    (add1 (apply max 4 (map ivl-midi∆ (map fingering-target (filter truth? chord-fingering))))))
  (apply beside
    (for/list ([sf (in-list chord-fingering)])
      (guitar-chord-chart/string/fingering sf n))))

;; guitar-chord-chart/string : [Maybe Interval] Nat -> Image
(define (guitar-chord-chart/string ivl n)
  (cond
    [(false? ivl)
     (apply above
       (overlay MUTE-STRING NUT-BOX)
       (for/list ([i (in-range 1 n)])
         FRET-BOX))]
    [(zero? (ivl-midi∆ ivl))
     (apply above
       (overlay OPEN-CIRCLE NUT-BOX)
       (for/list ([i (in-range 1 n)])
         FRET-BOX))]
    [else
     (apply above
       NUT-BOX
       (for/list ([i (in-range 1 n)])
         (if (= i (ivl-midi∆ ivl))
             (overlay CLOSED-CIRCLE FRET-BOX)
             FRET-BOX)))]))

;; guitar-chord-chart/string/fingering : StringFingering Nat -> Image
(define (guitar-chord-chart/string/fingering sf n)
  (cond
    [(false? sf)
     (apply above
       (overlay MUTE-STRING NUT-BOX)
       (for/list ([i (in-range 1 n)])
         FRET-BOX))]
    [(zero? (ivl-midi∆ (fingering-target sf)))
     (unless (false? (fingering-finger sf)) (error 'bad))
     (apply above
       (overlay OPEN-CIRCLE NUT-BOX)
       (for/list ([i (in-range 1 n)])
         FRET-BOX))]
    [else
     (define f (number->string (finger-num (fingering-finger sf))))
     (apply above
       NUT-BOX
       (for/list ([i (in-range 1 n)])
         (if (= i (ivl-midi∆ (fingering-target sf)))
             (overlay (text f FINGER-NUM-FONT-SIZE FINGER-NUM-COLOR)
                      CLOSED-CIRCLE
                      FRET-BOX)
             FRET-BOX)))]))

;; ------------------------------------------------------------------------

(module+ demo
  (define (guitar-charts lol)
    (for ([l (in-list lol)])
      (printf "~v " (guitar-chord-chart l)))
    (newline))

  (printf "~a\n" (make-string 70 #\-))
  (guitar-charts
   (list
    guitar-standard-E
    guitar-standard-A
    guitar-standard-D
    guitar-standard-G
    guitar-standard-C
    guitar-standard-F))

  (printf "~a\n" (make-string 70 #\-))
  (guitar-charts
   (list
    guitar-standard-Bm
    guitar-standard-Em
    guitar-standard-Am
    guitar-standard-Dm))

  (printf "~a\n" (make-string 70 #\-))
  (for-each guitar-charts
   (list
    ;; TODO: What other qualities of chords should be considered? Minimum
    ;; stretch isn't always the best, and when there are multiple with the
    ;; same stretch, what should decide between them?
    (min-stretch-chord-layouts guitar-strings (chord E2 major-triad))
    (min-stretch-chord-layouts guitar-strings (chord A2 major-triad))
    (min-stretch-chord-layouts guitar-strings (chord D3 major-triad))
    (min-stretch-chord-layouts guitar-strings (chord G2 major-triad))
    (min-stretch-chord-layouts guitar-strings (chord C3 major-triad))
    (min-stretch-chord-layouts guitar-strings (chord F3 major-triad))))

  (printf "~a\n" (make-string 70 #\-))
  (for-each guitar-charts
   (list
    ;; TODO: What other qualities of chords should be considered? Minimum
    ;; stretch isn't always the best, and when there are multiple with the
    ;; same stretch, what should decide between them?
    (min-stretch-chord-layouts guitar-strings (chord B2 minor-triad))
    (min-stretch-chord-layouts guitar-strings (chord E2 minor-triad))
    (min-stretch-chord-layouts guitar-strings (chord A2 minor-triad))
    (min-stretch-chord-layouts guitar-strings (chord D3 minor-triad))))

  (printf "~a\n" (make-string 70 #\-))
  (guitar-chord-chart/fingering
   (list
    (fingering L1 m2nd)
    (fingering L3 m3rd)
    (fingering L4 m3rd)
    (fingering L2 M2nd)
    (fingering L1 m2nd)
    (fingering L1 m2nd)))

  (printf "~a\n" (make-string 70 #\-)))

;; ------------------------------------------------------------------------

