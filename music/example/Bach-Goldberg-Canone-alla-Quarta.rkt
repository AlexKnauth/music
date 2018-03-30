#lang agile

(provide Bach-Goldberg-Canone-alla-Quarta)

(require music/data/time/main
         music/data/note/main
         music/data/scale/main
         music/data/score/main
         (submod music/data/note/note example)
         (submod music/data/scale/scale-note example)
         (submod music/data/scale/scale-note-held example))

(define (transform/time sorted-notes f)
  (for/list ([n (in-list sorted-notes)])
    (set-position n (f (get-position n)))))

(define (transform/note sorted-notes f)
  (for/list ([n (in-list sorted-notes)])
    (match n
      [(timed tp n)
       (timed tp (f n tp))])))

;; From BWV 988: Goldberg Variations, Variation 12, Canone alla Quarta

(define melody
  (sorted/time-period
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
  (sorted/time-period
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
   (λ (note pos)
     (with-scale (scale G1 major)
       (scale-note-octave+
        (match note
          [(scale-note d alteration)
           (define d* (diatonic-invert/around d (scale-note-diatonic s2:3)))
           (cond
             [(and (position=? (time-period-start pos) (position 2 beat-three))
                   (equal? d* (scale-note-diatonic s3:2)))
              (scale-note d* 1)]
             [else
              (scale-note d* 0)])])
        -1)))))

;; ------------------------------------------------------------------------

(define Bach-Goldberg-Canone-alla-Quarta
  (with-scale (scale G1 major)
    (score
     (metadata
      (work "Goldberg Variations")
      12
      "Canone alla Quarta"
      (creator "Bach"))
     (list
      (part "Melody"
            (append
             (here (position 0 beat-one)
               TREBLE-CLEF
               (key 1)
               (time-sig/nd 3 duration-quarter)
               (tempo 80 duration-quarter))
             (for/list ([melody (in-list melody)])
               (timed-map melody
                          scale-note->note))))
      (part "Melody-Transformed"
            (append
             (here (position 0 beat-one)
               TREBLE-CLEF
               (key 1)
               (time-sig/nd 3 duration-quarter)
               (tempo 80 duration-quarter))
             (for/list ([melody-transformed (in-list melody-transformed)])
               (timed-map melody-transformed
                          scale-note->note))))
      (part "Bass"
            (append
             (here (position 0 beat-one)
               BASS-CLEF
               (key 1)
               (time-sig/nd 3 duration-quarter)
               (tempo 80 duration-quarter))
             (for/list ([bass (in-list bass)])
               (timed-map bass
                          scale-note->note))))))))

;; ------------------------------------------------------------------------

