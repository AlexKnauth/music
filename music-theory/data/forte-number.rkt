#lang agile

(require "note.rkt"
         "pitch-class-set.rkt")
(module+ example
  (provide (all-defined-out)))
(module+ test
  (require rackunit
           (submod ".." example))
  (define-check (check-forte-number pcs fn)
    (check-equal? (pcs-forte-number pcs) fn))
  (begin-for-syntax
    (define-syntax-class chk-forte-clause
      [pattern (~and clause [pcs:expr fn:expr])
        #:with check (syntax/loc #'clause (check-forte-number pcs fn))]))
  (define-simple-macro (check-forte-numbers clause:chk-forte-clause ...)
    (begin clause.check ...)))

;; ------------------------------------------------------------------------

;; A ForteNumber is a (forte-number Nat Bool Nat [Maybe (U 'A 'B)])
(struct forte-number [cardinal-number z? ordinal-number inversion]
  #:transparent)

;; forte-number->string : ForteNumber -> String
(define (forte-number->string fn)
  (match-define (forte-number card z? ord inv) fn)
  (format "~a-~a~a~a"
          card
          (if z? "z" "")
          (add1 ord)
          (match inv [#f ""] ['A "A"] ['B "B"])))
          

;; forte-number-table : [Hashof [Listof PitchClass] Nat]
;; A mapping from prime forms to ordinal numbers
(define forte-number-table (make-hash))

;; forte-z-table : [Hashof [Listof PitchClass] Bool]
(define forte-z-table (make-hash))

;; pitch-class-set-forte-number : PitchClassSet -> ForteNumber
(define (pitch-class-set-forte-number pcs)
  (define n (length pcs))
  (define-values [prime-form inversion]
    (pitch-class-set-prime-form/inversion pcs))
  (cond
    [(hash-has-key? forte-number-table prime-form)
     (forte-number n
                   (hash-ref forte-z-table prime-form)
                   (hash-ref forte-number-table prime-form)
                   inversion)]
    [else
     (define combs
       (for/list ([comb (in-combinations (range 12) n)]
                  #:when (equal? comb (pitch-class-set-prime-form comb)))
         comb))
     (define sorted-groups
       (group-by
        pitch-class-set-interval-vector
        (sort combs prime-form-forte>?)))
     (define sorted
       (for*/list ([i (in-range (apply max 0 (map length sorted-groups)))]
                   [group (in-list sorted-groups)]
                   #:when (< i (length group)))
         (define comb (list-ref group i))
         (hash-set! forte-z-table comb (< 1 (length group)))
         comb))
     (for ([comb (in-list sorted)]
           [i (in-naturals)])
       (hash-set! forte-number-table comb i))
     (forte-number n
                   (hash-ref forte-z-table prime-form)
                   (hash-ref forte-number-table prime-form)
                   inversion)]))

;; prime-form-forte>? : [Listof PitchClass] [Listof PitchClass] -> Bool
(define (prime-form-forte>? a b)
  (define av (pitch-class-set-interval-vector a))
  (define bv (pitch-class-set-interval-vector b))
  (or (interval-vector>? av bv)
    (and (equal? av bv)
         (pc-list-left-packing>? a b))))


;; ------------------------------------------------------------------------

(module+ example
  (define forte-0-1 (forte-number 0 #f 0 #f))

  (define forte-1-1 (forte-number 1 #f 0 #f))

  (define forte-2-1 (forte-number 2 #f 0 #f))
  (define forte-2-2 (forte-number 2 #f 1 #f))
  (define forte-2-3 (forte-number 2 #f 2 #f))
  (define forte-2-4 (forte-number 2 #f 3 #f))
  (define forte-2-5 (forte-number 2 #f 4 #f))
  (define forte-2-6 (forte-number 2 #f 5 #f))

  (define forte-3-1 (forte-number 3 #f 0 #f))
  (define forte-3-2A (forte-number 3 #f 1 'A))
  (define forte-3-2B (forte-number 3 #f 1 'B))
  (define forte-3-3A (forte-number 3 #f 2 'A))
  (define forte-3-3B (forte-number 3 #f 2 'B))
  (define forte-3-4A (forte-number 3 #f 3 'A))
  (define forte-3-4B (forte-number 3 #f 3 'B))
  (define forte-3-5A (forte-number 3 #f 4 'A))
  (define forte-3-5B (forte-number 3 #f 4 'B))
  (define forte-3-6 (forte-number 3 #f 5 #f))
  (define forte-3-7A (forte-number 3 #f 6 'A))
  (define forte-3-7B (forte-number 3 #f 6 'B))
  (define forte-3-8A (forte-number 3 #f 7 'A))
  (define forte-3-8B (forte-number 3 #f 7 'B))
  (define forte-3-9 (forte-number 3 #f 8 #f))
  (define forte-3-10 (forte-number 3 #f 9 #f))
  (define forte-3-11A (forte-number 3 #f 10 'A))
  (define forte-3-11B (forte-number 3 #f 10 'B))
  (define forte-3-12 (forte-number 3 #f 11 #f))

  (define forte-4-1 (forte-number 4 #f 0 #f))
  (define forte-4-2A (forte-number 4 #f 1 'A))
  (define forte-4-2B (forte-number 4 #f 1 'B))
  (define forte-4-3 (forte-number 4 #f 2 #f))
  (define forte-4-4A (forte-number 4 #f 3 'A))
  (define forte-4-4B (forte-number 4 #f 3 'B))
  (define forte-4-5A (forte-number 4 #f 4 'A))
  (define forte-4-5B (forte-number 4 #f 4 'B))
  (define forte-4-6 (forte-number 4 #f 5 #f))
  (define forte-4-7 (forte-number 4 #f 6 #f))
  (define forte-4-8 (forte-number 4 #f 7 #f))
  (define forte-4-9 (forte-number 4 #f 8 #f))
  (define forte-4-10 (forte-number 4 #f 9 #f))
  (define forte-4-11A (forte-number 4 #f 10 'A))
  (define forte-4-11B (forte-number 4 #f 10 'B))
  (define forte-4-12A (forte-number 4 #f 11 'A))
  (define forte-4-12B (forte-number 4 #f 11 'B))
  (define forte-4-13A (forte-number 4 #f 12 'A))
  (define forte-4-13B (forte-number 4 #f 12 'B))
  (define forte-4-14A (forte-number 4 #f 13 'A))
  (define forte-4-14B (forte-number 4 #f 13 'B))
  (define forte-4-z15A (forte-number 4 #t 14 'A))
  (define forte-4-z15B (forte-number 4 #t 14 'B))
  (define forte-4-16A (forte-number 4 #f 15 'A))
  (define forte-4-16B (forte-number 4 #f 15 'B))
  (define forte-4-17 (forte-number 4 #f 16 #f))
  (define forte-4-18A (forte-number 4 #f 17 'A))
  (define forte-4-18B (forte-number 4 #f 17 'B))
  (define forte-4-19A (forte-number 4 #f 18 'A))
  (define forte-4-19B (forte-number 4 #f 18 'B))
  (define forte-4-20 (forte-number 4 #f 19 #f))
  (define forte-4-21 (forte-number 4 #f 20 #f))
  (define forte-4-22A (forte-number 4 #f 21 'A))
  (define forte-4-22B (forte-number 4 #f 21 'B))
  (define forte-4-23 (forte-number 4 #f 22 #f))
  (define forte-4-24 (forte-number 4 #f 23 #f))
  (define forte-4-25 (forte-number 4 #f 24 #f))
  (define forte-4-26 (forte-number 4 #f 25 #f))
  (define forte-4-27A (forte-number 4 #f 26 'A))
  (define forte-4-27B (forte-number 4 #f 26 'B))
  (define forte-4-28 (forte-number 4 #f 27 #f))
  (define forte-4-z29A (forte-number 4 #t 28 'A))
  (define forte-4-z29B (forte-number 4 #t 28 'B))

  (define forte-5-1 (forte-number 5 #f 0 #f))
  (define forte-5-2A (forte-number 5 #f 1 'A))
  (define forte-5-2B (forte-number 5 #f 1 'B))
  (define forte-5-3A (forte-number 5 #f 2 'A))
  (define forte-5-3B (forte-number 5 #f 2 'B))
  (define forte-5-4A (forte-number 5 #f 3 'A))
  (define forte-5-4B (forte-number 5 #f 3 'B))
  (define forte-5-5A (forte-number 5 #f 4 'A))
  (define forte-5-5B (forte-number 5 #f 4 'B))
  (define forte-5-6A (forte-number 5 #f 5 'A))
  (define forte-5-6B (forte-number 5 #f 5 'B))
  (define forte-5-7A (forte-number 5 #f 6 'A))
  (define forte-5-7B (forte-number 5 #f 6 'B))
  (define forte-5-8 (forte-number 5 #f 7 #f))
  (define forte-5-9A (forte-number 5 #f 8 'A))
  (define forte-5-9B (forte-number 5 #f 8 'B))
  (define forte-5-10A (forte-number 5 #f 9 'A))
  (define forte-5-10B (forte-number 5 #f 9 'B))
  (define forte-5-11A (forte-number 5 #f 10 'A))
  (define forte-5-11B (forte-number 5 #f 10 'B))
  (define forte-5-z12 (forte-number 5 #t 11 #f))
  (define forte-5-13A (forte-number 5 #f 12 'A))
  (define forte-5-13B (forte-number 5 #f 12 'B))
  (define forte-5-14A (forte-number 5 #f 13 'A))
  (define forte-5-14B (forte-number 5 #f 13 'B))
  (define forte-5-15 (forte-number 5 #f 14 #f))
  (define forte-5-16A (forte-number 5 #f 15 'A))
  (define forte-5-16B (forte-number 5 #f 15 'B))
  (define forte-5-z17 (forte-number 5 #t 16 #f))
  (define forte-5-z18A (forte-number 5 #t 17 'A))
  (define forte-5-z18B (forte-number 5 #t 17 'B))
  (define forte-5-19A (forte-number 5 #f 18 'A))
  (define forte-5-19B (forte-number 5 #f 18 'B))
  (define forte-5-20A (forte-number 5 #f 19 'A))
  (define forte-5-20B (forte-number 5 #f 19 'B))
  (define forte-5-21A (forte-number 5 #f 20 'A))
  (define forte-5-21B (forte-number 5 #f 20 'B))
  (define forte-5-22 (forte-number 5 #f 21 #f))
  (define forte-5-23A (forte-number 5 #f 22 'A))
  (define forte-5-23B (forte-number 5 #f 22 'B))
  (define forte-5-24A (forte-number 5 #f 23 'A))
  (define forte-5-24B (forte-number 5 #f 23 'B))
  (define forte-5-25A (forte-number 5 #f 24 'A))
  (define forte-5-25B (forte-number 5 #f 24 'B))
  (define forte-5-26A (forte-number 5 #f 25 'A))
  (define forte-5-26B (forte-number 5 #f 25 'B))
  (define forte-5-27A (forte-number 5 #f 26 'A))
  (define forte-5-27B (forte-number 5 #f 26 'B))
  (define forte-5-28A (forte-number 5 #f 27 'A))
  (define forte-5-28B (forte-number 5 #f 27 'B))
  (define forte-5-29A (forte-number 5 #f 28 'A))
  (define forte-5-29B (forte-number 5 #f 28 'B))
  (define forte-5-30A (forte-number 5 #f 29 'A))
  (define forte-5-30B (forte-number 5 #f 29 'B))
  (define forte-5-31A (forte-number 5 #f 30 'A))
  (define forte-5-31B (forte-number 5 #f 30 'B))
  (define forte-5-32A (forte-number 5 #f 31 'A))
  (define forte-5-32B (forte-number 5 #f 31 'B))
  (define forte-5-33 (forte-number 5 #f 32 #f))
  (define forte-5-34 (forte-number 5 #f 33 #f))
  (define forte-5-35 (forte-number 5 #f 34 #f))
  (define forte-5-z36A (forte-number 5 #t 35 'A))
  (define forte-5-z36B (forte-number 5 #t 35 'B))
  (define forte-5-z37 (forte-number 5 #t 36 #f))
  (define forte-5-z38A (forte-number 5 #t 37 'A))
  (define forte-5-z38B (forte-number 5 #t 37 'B))

  (define forte-6-1 (forte-number 6 #f 0 #f))

  )

;; ------------------------------------------------------------------------

(module+ test
  (define pcs-forte-number pitch-class-set-forte-number)

  (check-equal? (forte-number->string forte-2-5) "2-5")
  (check-equal? (forte-number->string forte-3-8A) "3-8A")
  (check-equal? (forte-number->string forte-3-11B) "3-11B")
  (check-equal? (forte-number->string forte-4-z15A) "4-z15A")
  (check-equal? (forte-number->string forte-4-z15B) "4-z15B")
  (check-equal? (forte-number->string forte-4-z29A) "4-z29A")
  (check-equal? (forte-number->string forte-4-z29B) "4-z29B")
  (check-equal? (forte-number->string forte-5-15) "5-15")
  (check-equal? (forte-number->string forte-5-z17) "5-z17")

  (check-forte-numbers
   ['() forte-0-1])

  (check-forte-numbers
   ['(0) forte-1-1])

  (check-forte-numbers
   ['(0 1) forte-2-1]
   ['(0 2) forte-2-2]
   ['(0 3) forte-2-3]
   ['(0 4) forte-2-4]
   ['(0 5) forte-2-5]
   ['(0 6) forte-2-6]
   ['(0 7) forte-2-5]
   ['(0 8) forte-2-4]
   ['(0 9) forte-2-3]
   ['(0 10) forte-2-2]
   ['(0 11) forte-2-1])

  (check-forte-numbers
   ['(0 1 2) forte-3-1]
   ['(0 1 3) forte-3-2A]
   ['(0 2 3) forte-3-2B]
   ['(0 1 4) forte-3-3A]
   ['(0 3 4) forte-3-3B]
   ['(0 1 5) forte-3-4A]
   ['(0 4 5) forte-3-4B]
   ['(0 1 6) forte-3-5A]
   ['(0 5 6) forte-3-5B]
   ['(0 2 4) forte-3-6]
   ['(0 2 5) forte-3-7A]
   ['(0 3 5) forte-3-7B]
   ['(0 2 6) forte-3-8A]
   ['(0 4 6) forte-3-8B]
   ['(0 2 7) forte-3-9]
   ['(0 3 6) forte-3-10]
   ['(0 3 7) forte-3-11A]
   ['(0 4 7) forte-3-11B]
   ['(0 4 8) forte-3-12])

  (check-forte-numbers
   ['(0 1 2 3) forte-4-1]
   ['(0 1 2 4) forte-4-2A]
   ['(0 2 3 4) forte-4-2B]
   ['(0 1 3 4) forte-4-3]
   ['(0 1 2 5) forte-4-4A]
   ['(0 3 4 5) forte-4-4B]
   ['(0 1 2 6) forte-4-5A]
   ['(0 4 5 6) forte-4-5B]
   ['(0 1 2 7) forte-4-6]
   ['(0 1 4 5) forte-4-7]
   ['(0 1 5 6) forte-4-8]
   ['(0 1 6 7) forte-4-9]
   ['(0 2 3 5) forte-4-10]
   ['(0 1 3 5) forte-4-11A]
   ['(0 2 4 5) forte-4-11B]
   ['(0 2 3 6) forte-4-12A]
   ['(0 3 4 6) forte-4-12B]
   ['(0 1 3 6) forte-4-13A]
   ['(0 3 5 6) forte-4-13B]
   ['(0 2 3 7) forte-4-14A]
   ['(0 4 5 7) forte-4-14B]
   ['(0 1 4 6) forte-4-z15A]
   ['(0 2 5 6) forte-4-z15B]
   ['(0 1 5 7) forte-4-16A]
   ['(0 2 6 7) forte-4-16B]
   ['(0 3 4 7) forte-4-17]
   ['(0 1 4 7) forte-4-18A]
   ['(0 3 6 7) forte-4-18B]
   ['(0 1 4 8) forte-4-19A]
   ['(0 3 4 8) forte-4-19B]
   ['(0 1 5 8) forte-4-20]
   ['(0 2 4 6) forte-4-21]
   ['(0 2 4 7) forte-4-22A]
   ['(0 3 5 7) forte-4-22B]
   ['(0 2 5 7) forte-4-23]
   ['(0 2 4 8) forte-4-24]
   ['(0 2 6 8) forte-4-25]
   ['(0 3 5 8) forte-4-26]
   ['(0 2 5 8) forte-4-27A]
   ['(0 3 6 8) forte-4-27B]
   ['(0 3 6 9) forte-4-28]
   ['(0 1 3 7) forte-4-z29A]
   ['(0 4 6 7) forte-4-z29B])

  (check-forte-numbers
   ['(0 1 2 3 4) forte-5-1]
   ['(0 1 2 3 5) forte-5-2A]
   ['(0 2 3 4 5) forte-5-2B]
   ['(0 1 2 4 5) forte-5-3A]
   ['(0 1 3 4 5) forte-5-3B]
   ['(0 1 2 3 6) forte-5-4A]
   ['(0 3 4 5 6) forte-5-4B]
   ['(0 1 2 3 7) forte-5-5A]
   ['(0 4 5 6 7) forte-5-5B]
   ['(0 1 2 5 6) forte-5-6A]
   ['(0 1 4 5 6) forte-5-6B]
   ['(0 1 2 6 7) forte-5-7A]
   ['(0 1 5 6 7) forte-5-7B]
   ['(0 2 3 4 6) forte-5-8]
   ['(0 1 2 4 6) forte-5-9A]
   ['(0 2 4 5 6) forte-5-9B]
   ['(0 1 3 4 6) forte-5-10A]
   ['(0 2 3 5 6) forte-5-10B]
   ['(0 2 3 4 7) forte-5-11A]
   ['(0 3 4 5 7) forte-5-11B]
   ['(0 1 3 5 6) forte-5-z12]
   ['(0 1 2 4 8) forte-5-13A]
   ['(0 2 3 4 8) forte-5-13B]
   ['(0 1 2 5 7) forte-5-14A]
   ['(0 2 5 6 7) forte-5-14B]
   ['(0 1 2 6 8) forte-5-15]
   ['(0 1 3 4 7) forte-5-16A]
   ['(0 3 4 6 7) forte-5-16B]
   ['(0 1 3 4 8) forte-5-z17]
   ['(0 1 4 5 7) forte-5-z18A]
   ['(0 2 3 6 7) forte-5-z18B]
   ['(0 1 3 6 7) forte-5-19A]
   ['(0 1 4 6 7) forte-5-19B]
   ['(0 1 5 6 8) forte-5-20A]
   ['(0 2 3 7 8) forte-5-20B]
   ['(0 1 4 5 8) forte-5-21A]
   ['(0 3 4 7 8) forte-5-21B]
   ['(0 1 4 7 8) forte-5-22]
   ['(0 2 3 5 7) forte-5-23A]
   ['(0 2 4 5 7) forte-5-23B]
   ['(0 1 3 5 7) forte-5-24A]
   ['(0 2 4 6 7) forte-5-24B]
   ['(0 2 3 5 8) forte-5-25A]
   ['(0 3 5 6 8) forte-5-25B]
   ['(0 2 4 5 8) forte-5-26A]
   ['(0 3 4 6 8) forte-5-26B]
   ['(0 1 3 5 8) forte-5-27A]
   ['(0 3 5 7 8) forte-5-27B]
   ['(0 2 3 6 8) forte-5-28A]
   ['(0 2 5 6 8) forte-5-28B]
   ['(0 1 3 6 8) forte-5-29A]
   ['(0 2 5 7 8) forte-5-29B]
   ['(0 1 4 6 8) forte-5-30A]
   ['(0 2 4 7 8) forte-5-30B]
   ['(0 1 3 6 9) forte-5-31A]
   ['(0 2 3 6 9) forte-5-31B]
   ['(0 1 4 6 9) forte-5-32A]
   ['(0 3 5 8 9) forte-5-32B]
   ['(0 2 4 6 8) forte-5-33]
   ['(0 2 4 6 9) forte-5-34]
   ['(0 2 4 7 9) forte-5-35]
   ['(0 1 2 4 7) forte-5-z36A]
   ['(0 3 5 6 7) forte-5-z36B]
   ['(0 3 4 5 8) forte-5-z37]
   ['(0 1 2 5 8) forte-5-z38A]
   ['(0 3 6 7 8) forte-5-z38B])

  (check-forte-numbers
   ['(0 1 2 3 4 5) forte-6-1]
   )
  )

;; ------------------------------------------------------------------------

