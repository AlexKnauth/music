#lang agile

;; Based on the segment labeling algorithm from the paper:

;; Algorithms for Chordal Analysis
;; by Bryan Pardo and William P. Birmingham
;; published in the Computer Music Journal on July 23, 2002

(require racket/dict
         music-theory/util/filter-maximal
         (prefix-in nc/ "../note/note-class.rkt")
         "../note/main.rkt"
         "../time/main.rkt"
         "../chord/chord-symbol.rkt"
         "../score/main.rkt"
         "../scale/scale.rkt"
         (submod "../note/note.rkt" example)
         (submod "../chord/chord-symbol.rkt" example))
(module+ test
  (require rackunit
           (submod "../note/note-held.rkt" example)))

;; ------------------------------------------------------------------------

(define standard-chord-symbol-kinds
  (list chord-symbol-kind:major
        chord-symbol-kind:minor
        chord-symbol-kind:diminished
        ;; TODO: These 2 are not in Table 2 of
        ;;       Algorithms for Chordal Analysis
        ;chord-symbol-kind:major-seventh
        ;chord-symbol-kind:minor-seventh
        chord-symbol-kind:dominant
        chord-symbol-kind:diminished-seventh
        ;; TODO: uncomment when this doesn't ruin everything
        ;chord-symbol-kind:half-diminished
        ))

;; ------------------------------------------------------------------------

(provide analyze-chords)

;; A BeatStrengths is a [Listof [Pair Duration Real]]

;; analyze-chords : Score -> [Listof [Timed [Maybe ChordSymbol]]]
(define (analyze-chords s)
  (let loop ([ms (score-notes-split-measures s)]
             [i 0]
             [k (key 0)]
             [ts #f]
             [acc '()])
    (match ms
      ['() (reverse acc)]
      [(cons m ms)
       (define key
         (or
          (for/last ([e (in-list m)]
                     #:when (key-there? e))
            (timed-value e))
          k))
       (define time-sig
         (or
          (for/last ([e (in-list m)]
                     #:when (time-sig-there? e))
            (timed-value e))
          ts))
       (loop
        ms
        (add1 i)
        key
        time-sig
        (cons (timed (time-period (position i beat-one)
                                  (time-sig-measure-length time-sig))
                     (analyze-chords/measure m key))
              acc))])))

;; ------------------------------------------------------------------------

;; analyze-chords/measure :
;; [Listof MusElementThere] Nat -> [Maybe ChordSymbol]
(define (analyze-chords/measure notes key)
  (analyze-chord/segment (filter note-there? notes)
                         key))

;; ------------------------------------------------------------------------

;; Algorithm in Figure 3 of Algorithms for Chordal Analysis:

;; --------------------------------------------------------------------
;; | 1. Determine the weight of each note in the segment by counting  |
;; |    the number of minimal segments in which the note is present   |
;; |    between the start and end of the current segment.             |
;; |                                                                  |
;; | 2. Sum the weights of the notes whose pitch class matches a      |
;; |    template element. Call this the Positive Evidence, `P`.       |
;; |                                                                  |
;; | 3. Sum the weights of the notes not matching any template        |
;; |    element. Call this Negative Evidence, `N`.                    |
;; |                                                                  |
;; | 4. Sum the count of template elements not matched by any note.   |
;; |    Call these the Misses, `M`.                                   |
;; |                                                                  |
;; | 5. Calculate the score for a template, `S`, with the formula     |
;; |    `S = P - (M + N)`                                             |
;; --------------------------------------------------------------------

(provide analyze-chord/segment
         analyze-chord/note-weights)

;; A NoteWeights is a [Hashof Note Nat]

;; analyze-chord/segment :
;; [Listof NoteThere] Key -> [Maybe ChordSymbol]
(define (analyze-chord/segment notes key)
  (cond
    [(empty? notes) #false]
    [else
     (analyze-chord/note-weights
       (note-weights/minimal-segments notes)
       (get-position (first notes))
       key)]))

;; analyze-chord/note-weights :
;; NoteWeights Position Key -> ChordSymbol
(define (analyze-chord/note-weights nws pos key)
  ;; ns : [Listof Note]
  (define ns
    (remove-duplicates (sort (hash-keys nws) note-midi<?)
                       note-class=?))
  (define bass-n (first ns))
  ;; chord-templates : [Listof ChordSymbol]
  (define chord-templates
    (for*/list ([n (in-list ns)]
                [kind (in-list standard-chord-symbol-kinds)])
      (chord-symbol (note-place-below n bass-n) kind)))
  (define (root-weight template)
    (define root (chord-symbol-root template))
    (for/sum ([(n w) (in-hash nws)]
              #:when (note-class=? n root))
      w))
  ;(printf "template-weights:\n")
  ;(for ([p (in-list (take template-weights 3))])
  ;  (match-define (cons t w) p)
  ;  (printf "  ~a: ~v\n" (map note->string t) w))
  (define maximal-templates
    (filter-maximal
     (filter-maximal
      (filter-maximal chord-templates < (score-chord-template nws))
      <
      root-weight)
     <
     (score-chord-template
      (for/hash ([n (in-list (key-notes key))])
        (values n 1)))))
  (cond
    [(= 1 (length maximal-templates))
     (first maximal-templates)]
    [else
     (error 'analyze-chords
       "multiple matches\n  templates: ~a\n  position: ~v"
       (map (λ (x) (map note->string x)) maximal-templates)
       pos)]))

;; note-weights-max-n : NoteWeights Nat -> [Listof Note]
(define (note-weights-max-n ns/ws num)
  ;; ns/ws-assoc : [Listof [Pair Note Nat]]
  (define ns/ws-assoc
    (sort (hash->list ns/ws) > #:key cdr))
  (map car (take ns/ws-assoc (min num (hash-count ns/ws)))))

;; note-place-below : Note Note -> Note
(define (note-place-below a b)
  (cond [(note-midi<=? a b) a]
        [else (note-place-below (note-octave+ a -1) b)]))

(module+ test
  (check-equal? (analyze-chord/segment
                 (list (timed/pos (position 0 beat-one) C4♩)
                       (timed/pos (position 0 beat-one) E4♩)
                       (timed/pos (position 0 beat-one) G4♩))
                 (key 0))
                (chord-symbol C4 chord-symbol-kind:major))
  )

;; ------------------------------------------------------------------------

;; Scoring Chord Templates

;; score-chord-template : NoteWeights -> [ChordSymbol -> Nat]
(define ((score-chord-template nws) template)
  (define chord (chord-symbol->chord template))
  (define root (chord-symbol-root template))
  (define P
    (for/sum ([(n w) (in-hash nws)]
              #:when (member n chord note-class=?))
      ;; This `if` and `add1` is added. It is not from the original
      ;; algorithm in the paper.
      (if (note-class=? n root)
          (add1 w)
          w)))
  (define N
    (for/sum ([(n w) (in-hash nws)]
              #:when (not (member n chord note-class=?)))
      w))
  (define M
    (for/sum ([n (in-list chord)]
              #:when (not (hash-has-key? nws n)))
      1))
  (- P (+ M N)))

;; ------------------------------------------------------------------------

;; Calculating Note Weights within a Segment

(provide note-weights/minimal-segments)

;; note-weights/minimal-segments :
;; [Listof NoteThere] -> NoteWeights
(define (note-weights/minimal-segments nts)
  (define partition-points (notes-there-partition-points nts))
  (for/fold ([hsh (hash)])
            ([nt (in-list nts)])
    (match-define (timed _ n) nt)
    (define w (note-there-count-contains nt partition-points))
    (hash-update hsh
                 n
      (λ (weight) (+ weight w))
      (λ () 0))))

;; notes-there-partition-points : [Listof NoteThere] -> [Listof Position]
;; Do not account for measure length (note-there-contains doesn't)
(define (notes-there-partition-points nts)
  (remove-duplicates
   (sorted/position
    (for/list ([nt (in-list nts)])
      (match nt
        [(timed (time-period start dur) n)
         (list start (position+ start dur))])))))

;; note-there-count-contains : NoteThere [Listof Position] -> Nat
(define (note-there-count-contains nt ps)
  (for/sum ([p (in-list ps)]
            #:when (note-there-contains? nt p))
    1))

;; note-there-contains? : NoteThere Position -> Bool
;; Start-inclusive, end-exclusive
;; Do not account for measure length (notes-there-partition-points doesn't)
(define (note-there-contains? nt pos)
  (match nt
    [(timed (time-period start dur) n)
     (and (position<=? start pos)
          (position<? pos (position+ start dur)))]))

;; ------------------------------------------------------------------------

(provide score-notes-split-measures)

;; score-notes-split-measures : Score -> [Listof [Listof MusElementThere]]
(define (score-notes-split-measures s)
  (notes-split-measures (score-notes s)))

;; score-notes : Score -> [Listof MusElementThere]
(define (score-notes s)
  (match s
    [(score _ (list (part _ notess) ...))
     (append* notess)]))

;; notes-split-measures : [Listof NoteThere] -> [Listof [Listof NoteThere]]
(define (notes-split-measures notes)
  (define groups
    (group-by position-measure-number notes))
  (let loop ([i 0] [groups groups] [acc '()])
    (match groups
      ['() (reverse acc)]
      [(cons fst _)
       #:when (< i (position-measure-number (first fst)))
       (loop (add1 i) groups (cons '() acc))]
      [(cons fst rst)
       (loop (add1 i) rst (cons fst acc))])))

;; key-signature->major-scale : Key -> Scale
(define (key-signature->major-scale k)
  (match (key-fifths k)
    [0 (scale C4 major)]
    [1 (scale G4 major)]
    [2 (scale D4 major)]
    [3 (scale A4 major)]
    [4 (scale E4 major)]
    [5 (scale B4 major)]
    [6 (scale F#4 major)]
    [-1 (scale F4 major)]
    [-2 (scale B♭4 major)]
    [-3 (scale E♭4 major)]
    [-4 (scale A♭4 major)]
    [-5 (scale D♭4 major)]
    [-6 (scale G♭4 major)]))

;; key-notes : Key -> [Listof Note]
(define (key-notes key)
  (scale-notes (key-signature->major-scale key)))

;; ------------------------------------------------------------------------

