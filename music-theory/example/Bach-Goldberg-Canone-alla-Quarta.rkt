#lang agile

(require "../data/note.rkt"
         "../data/scale/scale-note.rkt"
         "../data/scale/scale-note-held.rkt"
         "../data/note-held.rkt"
         "../data/position.rkt"
         "../data/score/score.rkt"
         (submod "../data/note.rkt" example)
         (submod "../data/note-held.rkt" example)
         (submod "../data/scale/scale-note.rkt" example)
         (submod "../data/scale/scale-note-held.rkt" example))
(module+ test
  (require racket/runtime-path
           "../notation/musicxml/musicxml-file.rkt"
           "../notation/musicxml/score.rkt"))

(define (transform/time sorted-notes f)
  (for/list ([n (in-list sorted-notes)])
    (set-position n (f (get-position n)))))

(define (transform/note sorted-notes f)
  (for/list ([n (in-list sorted-notes)])
    (match n
      [(with-pos pos (scale-note-held n d))
       (with-pos pos (scale-note-held (f n) d))])))

;; From BWV 988: Goldberg Variations, Variation 12, Canone alla Quarta

(define melody
  (sorted-notes
   (here (position 0 beat-one/and)   s0:3𝅘𝅥𝅯)
   (here (position 0 beat-one/a)     s6:2𝅘𝅥𝅯)
   (here (position 0 beat-two)       s0:3♪)
   (here (position 0 beat-two/and)   s1:3𝅘𝅥𝅯)
   (here (position 0 beat-two/a)     s2:3𝅘𝅥𝅯)
   (here (position 0 beat-three)     s3:3𝅘𝅥𝅯)
   (here (position 0 beat-three/e)   s2:3𝅘𝅥𝅯)
   (here (position 0 beat-three/and) s1:3𝅘𝅥𝅯)
   (here (position 0 beat-three/a)   s0:3𝅘𝅥𝅯)
   (here (position 1 beat-one)       s4:3𝅘𝅥𝅯)
   (here (position 1 beat-one/e)     s1:3𝅘𝅥𝅯)
   (here (position 1 beat-one/and)   s2:3𝅘𝅥𝅯)
   (here (position 1 beat-one/a)     s3#:3𝅘𝅥𝅯)
   (here (position 1 beat-two)       s4:3𝅘𝅥𝅯)
   (here (position 1 beat-two/e)     s5:3𝅘𝅥𝅯)
   (here (position 1 beat-two/and)   s6:3𝅘𝅥𝅯)
   (here (position 1 beat-two/a)     s0:4𝅘𝅥𝅯)
   (here (position 1 beat-three)     s1:4♪)
   (here (position 2 beat-one/and)   s0:4𝅘𝅥𝅯)
   (here (position 2 beat-one/a)     s6:3𝅘𝅥𝅯)
   (here (position 2 beat-two)       s5:3𝅘𝅥𝅯)
   (here (position 2 beat-two/e)     s4:3𝅘𝅥𝅯)
   (here (position 2 beat-two/and)   s3#:3𝅘𝅥𝅯)
   (here (position 2 beat-two/a)     s2:3𝅘𝅥𝅯)
   (here (position 2 beat-three)     s1:3𝅘𝅥𝅯)
   (here (position 2 beat-three/e)   s0:3𝅘𝅥𝅯)
   (here (position 2 beat-three/and) s6:2𝅘𝅥𝅯)
   (here (position 2 beat-three/a)   s0:3𝅘𝅥𝅯)
   (here (position 3 beat-one)       s0:3♪)
   (here (position 3 beat-one/and)   s6:2𝅘𝅥𝅯)
   (here (position 3 beat-one/a)     s5:2𝅘𝅥𝅯)
   (here (position 3 beat-two)       s4:2♪)
   (here (position 4 beat-one/e)     s4:3♪.) ;; TODO: tie
   (here (position 4 beat-two)       s4:3𝅘𝅥𝅯)
   (here (position 4 beat-two/e)     s2:3𝅘𝅥𝅯)
   (here (position 4 beat-two/and)   s1:3𝅘𝅥𝅯)
   (here (position 4 beat-two/a)     s0:3𝅘𝅥𝅯)
   (here (position 4 beat-three)     s6♭:2𝅘𝅥𝅯)
   (here (position 4 beat-three/e)   s5:2𝅘𝅥𝅯)
   (here (position 4 beat-three/and) s4:2𝅘𝅥𝅯)
   (here (position 4 beat-three/a)   s6♭:2𝅘𝅥𝅯)
   (here (position 5 beat-one)       s5:2𝅘𝅥𝅯)
   (here (position 5 beat-one/e)     s0:3𝅘𝅥𝅯)
   (here (position 5 beat-one/and)   s3:3𝅘𝅥𝅯)
   (here (position 5 beat-one/a)     s4:3𝅘𝅥𝅯)
   (here (position 5 beat-two)       s5:3♪)
   (here (position 5 beat-two/and)   s1:3♩)
   (here (position 5 beat-three/and) s1:3♪) ; TODO: tie
   (here (position 6 beat-one)       s1:3𝅘𝅥𝅯)
   (here (position 6 beat-one/e)     s4:2𝅘𝅥𝅯)
   (here (position 6 beat-one/and)   s6:2𝅘𝅥𝅯)
   (here (position 6 beat-one/a)     s0:3𝅘𝅥𝅯)
   (here (position 6 beat-two)       s1:3𝅘𝅥𝅯)
   (here (position 6 beat-two/e)     s0:3𝅘𝅥𝅯)
   (here (position 6 beat-two/and)   s6:2𝅘𝅥𝅯)
   (here (position 6 beat-two/a)     s5:3𝅘𝅥𝅯)
   (here (position 6 beat-three)     s4:3𝅘𝅥𝅯)
   (here (position 6 beat-three/e)   s3:3𝅘𝅥𝅯)
   (here (position 6 beat-three/and) s2:3𝅘𝅥𝅯)
   (here (position 6 beat-three/a)   s1:3𝅘𝅥𝅯)
   (here (position 7 beat-one)       s0:3♩)
   ))

(define bass
  (sorted-notes
   (here (position 0 beat-one)       s0:2♩)
   (here (position 0 beat-two)       s0:2♩)
   (here (position 0 beat-three)     s0:2♩)
   (here (position 1 beat-one)       s6:1♩)
   (here (position 1 beat-two)       s6:1♩)
   (here (position 1 beat-three)     s6:1♩)
   (here (position 2 beat-one)       s5:1♩)
   (here (position 2 beat-two)       s5:1♩)
   (here (position 2 beat-three)     s5:1♩)
   (here (position 3 beat-one)       s4:1𝅘𝅥𝅯)
   (here (position 3 beat-one/e)     s4:0𝅘𝅥𝅯)
   (here (position 3 beat-one/and)   s4:1♪)
   (here (position 3 beat-two/and)   s3:1𝅘𝅥𝅯)
   (here (position 3 beat-two/a)     s2:1𝅘𝅥𝅯)
   (here (position 3 beat-three)     s1:1♪)
   (here (position 3 beat-three/and) s3:1♪)
   (here (position 4 beat-one)       s2:1♩)
   (here (position 4 beat-two)       s2:1♩)
   (here (position 4 beat-three)     s2:1♩)
   (here (position 5 beat-one)       s3:1♩)
   (here (position 5 beat-two)       s3:1♩)
   (here (position 5 beat-three)     s3:1♩)
   (here (position 6 beat-one)       s4:1♩)
   (here (position 6 beat-two)       s4:1♩)
   (here (position 6 beat-three)     s4:1♩)
   (here (position 7 beat-one)       s0:2♪.)
   (here (position 7 beat-one/a)     s2:1𝅘𝅥𝅯)
   (here (position 7 beat-two)       s5:1𝅘𝅥𝅯)
   (here (position 7 beat-two/e)     s4:1𝅘𝅥𝅯)
   (here (position 7 beat-two/and)   s3:1𝅘𝅥𝅯)
   (here (position 7 beat-two/a)     s5:1𝅘𝅥𝅯)
   (here (position 7 beat-three)     s4:1𝅘𝅥𝅯)
   (here (position 7 beat-three/e)   s3:1𝅘𝅥𝅯)
   (here (position 7 beat-three/and) s2:1𝅘𝅥𝅯)
   (here (position 7 beat-three/a)   s1:1𝅘𝅥𝅯)
   (here (position 8 beat-one)       s0:1♩)
   ))

(define melody-transformed
  (transform/note
   (transform/time
    melody
    (λ (pos)
      (position-measure+ pos 1)))
   (λ (note)
     (with-scale (scale G1 major)
       (scale-note-octave+
        (match note
          [(scale-note d alteration)
           (scale-note (diatonic-invert/around d (scale-note-diatonic s2:3)) 0)])
        -1)))))

(define Bach-Goldberg-Canone-alla-Quarta
  (with-scale (scale G1 major)
    (score
     (key 1)
     (tempo 80 duration-quarter)
     (duration 3 1)
     (list
      (part "Melody"
            (for/list ([melody (in-list melody)])
              (with-pos-map melody
                            scale-note-held->note-held)))
      (part "Melody-Transformed"
            (for/list ([melody-transformed (in-list melody-transformed)])
              (with-pos-map melody-transformed
                            scale-note-held->note-held)))
      (part "Bass"
            (for/list ([bass (in-list bass)])
              (with-pos-map bass
                            scale-note-held->note-held)))))))

;; ------------------------------------------------------------------------

(module+ test
  (define-runtime-path Bach-Goldberg-Canone-alla-Quarta.xml
    "Bach-Goldberg-Canone-alla-Quarta.xml")

  (write-musicxml-file Bach-Goldberg-Canone-alla-Quarta.xml
                       (score->musicxml Bach-Goldberg-Canone-alla-Quarta)
                       #:exists 'replace)

  (open-musicxml-file/MuseScore-2 Bach-Goldberg-Canone-alla-Quarta.xml)
  )

;; ------------------------------------------------------------------------

