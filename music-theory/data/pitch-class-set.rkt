#lang agile

(require "note.rkt"
         math/number-theory
         (only-in data/collection find-best))
(module+ test
  (require rackunit))

;; ------------------------------------------------------------------------

;; Pitch Class Sets

;; A PitchClassSet is a [Listof PitchClass]

;; ------------------------------------------------------------------------

;; Prime Form

(provide pitch-class-set-prime-form
         pitch-class-set-prime-form/inversion)

(define (pitch-class-set-prime-form pcs)
  (define-values [prime-form inversion]
    (pitch-class-set-prime-form/inversion pcs))
  prime-form)

;; pitch-class-set-prime-form/inversion :
;; PitchClassSet -> (values [Listof PitchClass] [Maybe (U 'A 'B)])
(define (pitch-class-set-prime-form/inversion pcs)
  (define -pcs (pc-set-invert pcs))
  (define N1 (pc-list-transpose-zero (pitch-class-set-normal-form pcs)))
  (define N2 (pc-list-transpose-zero (pitch-class-set-normal-form -pcs)))
  (cond
    [(equal? N1 N2)
     (values N1 #f)]
    [(pc-list-left-packing>? N1 N2)
     (values N1 'A)]
    [else
     (values N2 'B)]))

;; pc-set-invert : PitchClassSet -> [Listof PitchClass]
(define (pc-set-invert pcs)
  (with-modulus 12 (map mod- pcs)))

;; pc-list-transpose-zero : [Listof PitchClass] -> [Listof PitchClass]
(define (pc-list-transpose-zero pc-lst)
  (cond
    [(empty? pc-lst) '()]
    [else
     (define n (first pc-lst))
     (with-modulus 12
       (for/list ([pc (in-list pc-lst)])
         (mod- pc n)))]))

(module+ test
  (check-equal? (pitch-class-set-prime-form '(0 4 7)) '(0 3 7))
  (check-equal? (pitch-class-set-prime-form '(0 5 9)) '(0 3 7))
  (check-equal? (pitch-class-set-prime-form '(11 2 7)) '(0 3 7))

  (check-equal? (pitch-class-set-prime-form '(2 9 10))
                '(0 1 5))

  (check-equal? (pitch-class-set-prime-form '(7 8 2 5))
                '(0 1 3 6))

  (check-equal? (pitch-class-set-prime-form '(5 4 10 7))
                '(0 1 3 6))

  (check-equal? (pitch-class-set-prime-form '(0 1 3 7 8))
                '(0 1 5 6 8))

  (check-equal? (pitch-class-set-prime-form '(0 1 3 6 8 9))
                '(0 2 3 6 7 9))

  (check-equal? (pitch-class-set-prime-form '(0 1 3 5 8 9))
                '(0 1 4 5 7 9))

  (check-equal? (pitch-class-set-prime-form '(0 1 2 4 7 8 9))
                '(0 1 2 5 6 7 9))

  (check-equal? (pitch-class-set-prime-form '(0 1 2 4 5 7 9 10))
                '(0 1 3 4 5 7 8 10)))

;; ------------------------------------------------------------------------

;; Normal Form

(provide pitch-class-set-normal-form)

;; pitch-class-set-normal-form : PitchClassSet -> PitchClassSet
;; Uses the Rahn method for comparing packings
(define (pitch-class-set-normal-form pcs)
  (cond
    [(empty? pcs) '()]
    [else
     (define pc-lst (sort pcs <))
     (define rotations
       (for/list ([i (in-range (length pc-lst))])
         (pc-list-rotate pc-lst i)))
     (find-best rotations pc-deltas-left-packing>? #:key pc-list-deltas)]))

;; pc-list-rotate : [Listof PitchClass] Nat -> [Listof PitchClass]
(define (pc-list-rotate pc-lst i)
  (define-values [a b]
    (split-at pc-lst i))
  (append b a))

;; ------------------------------------------------------------------------

(module+ test
  (check-equal? (pitch-class-set-normal-form '(0 4 7)) '(0 4 7))
  (check-equal? (pitch-class-set-normal-form '(0 5 9)) '(5 9 0))
  (check-equal? (pitch-class-set-normal-form '(11 2 7)) '(7 11 2))

  (check-equal? (pitch-class-set-normal-form '(2 9 10))
                '(9 10 2))

  (check-equal? (pitch-class-set-normal-form '(7 8 2 5))
                '(2 5 7 8))

  (check-equal? (pitch-class-set-normal-form '(5 4 10 7))
                '(4 5 7 10)))

;; ------------------------------------------------------------------------

;; Comparing Compactness of Packings

(provide pc-list-left-packing>?)

;; A PCDelta is an Int[0,11]

;; pc-list-deltas : [Listof PitchClass] -> [Listof PCDelta]
(define (pc-list-deltas pcs)
  (with-modulus 12
    (for/list ([pc1 (in-list pcs)]
               [pc2 (in-list (pc-list-rotate pcs 1))])
      (mod- pc2 pc1))))

;; pc-list-left-packing>? : [Listof PitchClass] [Listof PitchClass] -> Bool
;; Uses the Rahn method
(define (pc-list-left-packing>? a b)
  (pc-deltas-left-packing>? (pc-list-deltas a) (pc-list-deltas b)))

;; pc-deltas-left-packing>? : [Listof PCDelta] [Listof PCDelta] -> Bool
;; Uses the Rahn method
(define (pc-deltas-left-packing>? as bs)
  (let loop ([as (reverse as)] [bs (reverse bs)])
    (match* [as bs]
      [['() '()] #false]
      [[(cons a as) (cons b bs)]
       (or (> a b)
           (and (= a b)
                (loop as bs)))])))

;; ------------------------------------------------------------------------

