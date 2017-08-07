#lang agile

(require racket/bool
         music-theory/util/filter-maximal
         "../note/note.rkt"
         "../chord/chord.rkt"
         "fingering.rkt"
         "string-spec.rkt"
         (submod "fingering.rkt" example))
(module+ example
  (provide (all-defined-out)))
(module+ test
  (require rackunit
           (submod ".." example)
           (submod "../note/note.rkt" example)))

;; ------------------------------------------------------------------------

;; See the JGuitar Chord Calculator:
;; http://jguitar.com/chord

;; ------------------------------------------------------------------------

(provide chord-layout->chord)

;; ChordLayout is a [Listof [Maybe Interval]]
;; For a given StringSpec, each interval represents the interval of the
;; note above the string.

;; chord-layout->chord : StringSpec ChordLayout -> Chord
(define (chord-layout->chord strings layout)
  (for/list ([string (in-list strings)]
             [ivl (in-list layout)]
             #:when ivl)
    (note+ string ivl)))

(module+ example

  ;; ---------------------------------------------

  ;; Standard Major Chords

  (define guitar-standard-F
    (list #f #f m3rd M2nd m2nd m2nd))
  (define guitar-standard-F*
    (list m2nd m3rd m3rd M2nd m2nd m2nd))

  (define guitar-standard-C
    (list #f m3rd M2nd unison m2nd unison))

  (define guitar-standard-G
    (list m3rd M2nd unison unison unison m3rd))

  (define guitar-standard-D
    (list #f #f unison M2nd m3rd M2nd))

  (define guitar-standard-A
    (list #f unison M2nd M2nd M2nd unison))

  (define guitar-standard-E
    (list unison M2nd M2nd ivl-sharp unison unison))

  ;; ---------------------------------------------

  ;; Standard Minor Chords

  (define guitar-standard-Dm
    (list #f #f unison M2nd m3rd m2nd))

  (define guitar-standard-Am
    (list #f unison M2nd M2nd m2nd unison))

  (define guitar-standard-Em
    (list unison M2nd M2nd unison unison unison))

  (define guitar-standard-Bm
    (list #f M2nd M3rd M3rd m3rd M2nd))

  ;; ---------------------------------------------

  )

(module+ test
  (define-binary-check (check-chord actual expected)
    (chord-approx/bass+class=? actual expected))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-F)
               (chord F3 major-triad))
  (check-chord (chord-layout->chord guitar-strings guitar-standard-F*)
               (chord F2 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-C)
               (chord C3 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-G)
               (chord G2 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-D)
               (chord D3 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-A)
               (chord A2 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-E)
               (chord E2 major-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-Dm)
               (chord D3 minor-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-Am)
               (chord A2 minor-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-Em)
               (chord E2 minor-triad))

  (check-chord (chord-layout->chord guitar-strings guitar-standard-Bm)
               (chord B2 minor-triad))

  )

;; ------------------------------------------------------------------------

;; Generating Chord Layouts

(provide generate-chord-layouts)

;; generate-chord-layouts : StringSpec Chord -> [Listof ChordLayout]
;; The chord must be sorted from lowest note to highest
(define (generate-chord-layouts strings chord)
  (cond
    [(note-midi<? (first chord) (first strings))
     (generate-chord-layouts strings (chord-octave+ chord 1))]
    [else
     (reverse (generate-chord-layouts/acc strings chord 0 '()))]))

;; generate-chord-layouts/acc :
;; StringSpec Chord Nat [Listof ChordLayout] -> [Listof ChordLayout]
(define (generate-chord-layouts/acc strings chord n acc)
  ;; try it starting from every string
  (cond
    [(empty? strings) acc]
    [else
     (generate-chord-layouts/acc
      (rest strings)
      chord
      (add1 n)
      (generate-chord-layouts-from strings chord n acc))]))

;; generate-chord-layouts-from :
;; StringSpec Chord Nat [Listof ChordLayout] -> [Listof ChordLayout]
;; ASSUME strings is not empty
(define (generate-chord-layouts-from strings chord n acc)
  (cond
    [(note-midi<? (first chord) (first strings))
     acc]
    [else
     (continue-chord-layouts (rest strings) chord
       (cons (note∆ (first strings) (first chord))
             (make-list n #false))
       acc)]))

;; continue-chord-layouts :
;; StringSpec Chord [Listof Note] [Listof ChordLayout]
;; -> [Listof ChordLayout]
(define (continue-chord-layouts strings chord acc1 acc2)
  (match strings
    ['()
     (cons (reverse acc1) acc2)]
    [(cons fst rst)
     (for/fold ([acc2 acc2])
               ([next (in-list chord)])
       (continue-chord-layouts rst chord
         (cons (note-put-above∆ fst next) acc1)
         acc2))]))

;; note-put-above∆ : Note Note -> Interval
(define (note-put-above∆ base note)
  (cond [(and (note-midi<=? base note)
              (note-midi<=? note (note+ base octave)))
         (note∆ base note)]
        [(note-midi<=? base note)
         #;(note∆ base note)
         (note-put-above∆ base (note-octave+ note -1))]
        [else
         (note-put-above∆ base (note-octave+ note +1))]))

;; ------------------------------------------------------------------------

(module+ test
  (define-binary-check (check-element actual-list expected-member)
    (member expected-member actual-list))

  (check-element (generate-chord-layouts guitar-strings
                                         (chord E2 major-triad))
                 guitar-standard-E)

  (check-element (generate-chord-layouts guitar-strings
                                         (chord A2 major-triad))
                 guitar-standard-A)

  (check-element (generate-chord-layouts guitar-strings
                                         (chord D3 major-triad))
                 guitar-standard-D)

  (check-element (generate-chord-layouts guitar-strings
                                         (chord G2 major-triad))
                 guitar-standard-G)

  (check-element (generate-chord-layouts guitar-strings
                                         (chord C3 major-triad))
                 guitar-standard-C)

  (check-element (generate-chord-layouts guitar-strings
                                         (chord F3 major-triad))
                 guitar-standard-F)

  (check-element (generate-chord-layouts guitar-strings
                                         (chord F2 major-triad))
                 guitar-standard-F*)
  )

;; ------------------------------------------------------------------------

;; A ChordFingering is a [Listof StringFingering]
;; For a given StringSpec, each fingering of an interval represents the
;; interval of the note above the string along with which finger should
;; play that note.

;; A StringFingering is one of:
;;  - #false
;;  - [Fingeringof Interval]

;; chord-fingering->chord : StringSpec ChordFingering -> Chord
(define (chord-fingering->chord strings chord-fingering)
  (for/list ([string (in-list strings)]
             [string-fingering (in-list chord-fingering)]
             #:when string-fingering)
    (note+ string (fingering-target string-fingering))))

;; generate-chord-fingerings/layout : ChordLayout -> [Listof ChordFingering]
(define (generate-chord-fingerings/layout chord-layout)
  (define ns (filter values chord-layout))
  (cond [(< 4 (count ivl-midi-positive? ns))
         (generate-bar-chord-fingerings chord-layout L1 (list L2 L3 L4))]
        [else
         (append
          (generate-non-bar-chord-fingerings chord-layout (list L1 L2 L3 L4))
          (generate-bar-chord-fingerings chord-layout L1 (list L2 L3 L4)))]))

;; generate-bar-chord-fingerings :
;; ChordLayout -> [Listof ChordFingering]
(define (generate-bar-chord-fingerings chord-layout bar-finger fingers)
  (define ns (filter values chord-layout))
  (define ns+ (filter ivl-midi-positive? ns))
  (define lowest-fret (argmin ivl-midi∆ ns+))
  (define (=lowest? x) (ivl-midi=? x lowest-fret))
  (cond
    [(<= (count =lowest? ns+) 1)
     '()]
    [else
     (define new-layout
       (for/list ([s (in-list chord-layout)])
         (if (and s (=lowest? s))
             unison
             s)))
     (for/list ([cf (in-list
                     (generate-non-bar-chord-fingerings new-layout fingers))])
       (for/list ([sf (in-list cf)]
                  [si (in-list chord-layout)])
         (if (and si (=lowest? si))
             (fingering bar-finger si)
             sf)))]))

;; generate-non-bar-chord-fingerings :
;; ChordLayout [Listof Finger] -> [Listof ChordFingering]
(define (generate-non-bar-chord-fingerings chord-layout fingers)
  (match chord-layout
    ['() (list '())]
    [(cons #f chord-rst)
     (for/list ([cf (in-list
                     (generate-non-bar-chord-fingerings chord-rst fingers))])
       (cons #f cf))]
    [(cons chord-fst chord-rst)
     #:when (ivl-midi-zero? chord-fst)
     (for/list ([cf (in-list
                     (generate-non-bar-chord-fingerings chord-rst fingers))])
       (cons (fingering #false chord-fst) cf))]
    [(cons chord-fst chord-rst)
     (cond
       [(empty? fingers) '()]
       [else
        (append*
         (for/list ([f (in-list fingers)])
           (define fs (remove f fingers))
           (append
            (for/list ([cf (in-list
                            (generate-non-bar-chord-fingerings chord-rst fs))])
              (cons (fingering f chord-fst) cf))
            (generate-non-bar-chord-fingerings chord-layout fs))))])]))

(module+ test
  (check-equal? (generate-chord-fingerings/layout (list A4th P4th M3rd m3rd M2nd m2nd))
                '())

  (check-equal? (generate-chord-fingerings/layout (list m2nd m3rd m3rd M2nd m2nd m2nd))
                (list
                 (list (fingering L1 m2nd)
                       (fingering L2 m3rd)
                       (fingering L3 m3rd)
                       (fingering L4 M2nd)
                       (fingering L1 m2nd)
                       (fingering L1 m2nd))
                 (list (fingering L1 m2nd)
                       (fingering L2 m3rd)
                       (fingering L4 m3rd)
                       (fingering L3 M2nd)
                       (fingering L1 m2nd)
                       (fingering L1 m2nd))
                 (list (fingering L1 m2nd)
                       (fingering L3 m3rd)
                       (fingering L2 m3rd)
                       (fingering L4 M2nd)
                       (fingering L1 m2nd)
                       (fingering L1 m2nd))
                 (list (fingering L1 m2nd)
                       (fingering L3 m3rd)
                       (fingering L4 m3rd)
                       (fingering L2 M2nd)
                       (fingering L1 m2nd)
                       (fingering L1 m2nd))
                 (list (fingering L1 m2nd)
                       (fingering L4 m3rd)
                       (fingering L2 m3rd)
                       (fingering L3 M2nd)
                       (fingering L1 m2nd)
                       (fingering L1 m2nd))
                 (list (fingering L1 m2nd)
                       (fingering L4 m3rd)
                       (fingering L3 m3rd)
                       (fingering L2 M2nd)
                       (fingering L1 m2nd)
                       (fingering L1 m2nd)))))

;; ------------------------------------------------------------------------

;; Which chord fingerings are impossible to play?

;; chord-fingering-possible? : ChordFingering -> Bool
(define (chord-fingering-possible? chord-fingering)
  (and
   ;; TODO: What other criteria should there be?
   (chord-fingering-ascending-fingers? chord-fingering)))

;; chord-fingering-ascending-fingers? : ChordFingering -> Bool
(define (chord-fingering-ascending-fingers? chord-fingering)
  (define (finger<? a b)
    (cond [(and (finger? a) (finger? b))
           (< (finger-num a) (finger-num b))]
          [(and (false? a) (finger? b)) #true]
          [else #false]))
  (match chord-fingering
    ['() #true]
    [(cons #f cf)
     (chord-fingering-ascending-fingers? cf)]
    [(cons (fingering af aivl) cf)
     (and
      (for/and ([b (in-list cf)])
        (match-define (fingering bf bivl) b)
        (cond [(ivl-midi<? aivl bivl) (finger<? af bf)]
              [(ivl-midi<? bivl aivl) (finger<? bf af)]
              [else #true]))
      (chord-fingering-ascending-fingers? cf))]))

(module+ test
  (check-true  (chord-fingering-possible?
                (list #false
                      #false
                      (fingering #f unison)
                      (fingering L1 M2nd)
                      (fingering L3 m3rd)
                      (fingering L2 M2nd))))
  (check-true  (chord-fingering-possible?
                (list #false
                      #false
                      (fingering #f unison)
                      (fingering L2 M2nd)
                      (fingering L3 m3rd)
                      (fingering L1 M2nd))))

  (check-false (chord-fingering-possible?
                (list #false
                      #false
                      (fingering #f unison)
                      (fingering L1 M2nd)
                      (fingering L2 m3rd)
                      (fingering L3 M2nd))))
  (check-false (chord-fingering-possible?
                (list #false
                      #false
                      (fingering #f unison)
                      (fingering L2 M2nd)
                      (fingering L1 m3rd)
                      (fingering L3 M2nd))))
  (check-false (chord-fingering-possible?
                (list #false
                      #false
                      (fingering #f unison)
                      (fingering L3 M2nd)
                      (fingering L1 m3rd)
                      (fingering L2 M2nd))))
  (check-false (chord-fingering-possible?
                (list #false
                      #false
                      (fingering #f unison)
                      (fingering L3 M2nd)
                      (fingering L2 m3rd)
                      (fingering L1 M2nd))))

  (check-equal? (filter chord-fingering-possible?
                  (generate-non-bar-chord-fingerings
                    (list #f #f unison M2nd m3rd M2nd)
                    (list L1 L2 L3)))
                (list
                 (list #f
                       #f
                       (fingering #f unison)
                       (fingering L1 M2nd)
                       (fingering L3 m3rd)
                       (fingering L2 M2nd))
                 (list #f
                       #f
                       (fingering #f unison)
                       (fingering L2 M2nd)
                       (fingering L3 m3rd)
                       (fingering L1 M2nd))))

  (check-equal? (filter chord-fingering-possible?
                  (generate-chord-fingerings/layout
                    (list m2nd m3rd m3rd M2nd m2nd m2nd)))
                (list
                 (list (fingering L1 m2nd)
                       (fingering L3 m3rd)
                       (fingering L4 m3rd)
                       (fingering L2 M2nd)
                       (fingering L1 m2nd)
                       (fingering L1 m2nd))
                 (list (fingering L1 m2nd)
                       (fingering L4 m3rd)
                       (fingering L3 m3rd)
                       (fingering L2 M2nd)
                       (fingering L1 m2nd)
                       (fingering L1 m2nd))))
  )

;; ------------------------------------------------------------------------

;; Which chord layouts are impossible to play?

(provide chord-layout-stretch min-stretch-chord-layouts)

;; TODO: What other qualities of chords should be considered? Minimum
;; stretch isn't always the best, and when there are multiple with the
;; same stretch, what should decide between them?

;; chord-layout-stretch : ChordLayout -> Nat
;; For now, only works for guitar-like instruments.
;; Also, it treats fret distances the same whether they're down low where
;; the frets are in reality farther apart, or up high where they are
;; closer together.
(define (chord-layout-stretch chord-layout)
  (define ns
    (for/list ([ivl (in-list chord-layout)]
               #:when ivl)
      (ivl-midi∆ ivl)))
  (define fingered-ns (filter positive? ns))
  (cond [(empty? fingered-ns) 0]
        [else
         (- (apply max fingered-ns)
            (apply min fingered-ns))]))

;; chord-layout-stretch<=? : Nat -> [ChordLayout -> Bool]
(define ((chord-layout-stretch<=? n) chord-layout)
  (<= (chord-layout-stretch chord-layout) n))

;; min-stretch-chord-layouts : StringSpec Chord -> [Listof ChordLayout]
(define (min-stretch-chord-layouts strings chord)
  (filter-maximal
   ;(filter-maximal
    (filter (λ (x) (<= (chord-layout-stretch x) 3))
            (generate-chord-layouts strings chord))
   ; >
   ; number-of-black-dots)
   >
   highest-fret))

(define (number-of-black-dots chord-layout)
  (define ns
    (for/list ([ivl (in-list chord-layout)]
               #:when ivl)
      (ivl-midi∆ ivl)))
  (count positive? ns))

(define (highest-fret chord-layout)
  (for/fold ([acc 0])
            ([ivl (in-list chord-layout)]
             #:when ivl)
    (max acc (ivl-midi∆ ivl))))

(module+ test
  (check-equal? (min-stretch-chord-layouts guitar-strings
                                           (chord E2 major-triad))
                (list guitar-standard-E))

  (check-equal? (min-stretch-chord-layouts guitar-strings
                                          (chord A2 major-triad))
                (list guitar-standard-A))

  (check-equal? (min-stretch-chord-layouts guitar-strings
                                           (chord G2 major-triad))
                (list guitar-standard-G
                      (list m3rd M2nd unison unison m3rd m3rd)))

  ;; TODO: what should these tests be? Min-stretch isn't exactly what we
  ;; want here.
  (check-equal? (min-stretch-chord-layouts guitar-strings
                                           (chord D3 major-triad))
                (list guitar-standard-D))

  (check-equal? (min-stretch-chord-layouts guitar-strings
                                           (chord C3 major-triad))
                (list guitar-standard-C
                      (list #f m3rd M2nd unison m2nd m3rd)))

  (check-equal? (min-stretch-chord-layouts guitar-strings
                                           (chord F3 major-triad))
                (list guitar-standard-F))
  )

;; ------------------------------------------------------------------------

