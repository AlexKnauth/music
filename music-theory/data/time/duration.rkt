#lang agile

;; ------------------------------------------------------------------------

(provide duration
         duration-common-divisions
         duration-n/divisions
         duration=?
         duration<?
         duration<=?
         duration+
         duration∆
         duration-divide
         duration-fraction
         duration-zero
         duration-quarter
         duration-eighth
         duration-sixteenth
         duration-half
         duration-whole
         duration-dotted-quarter
         duration-dotted-eighth
         duration-dotted-half
         duration-dotted-whole)

;; A Duration is a (duration Nat PosInt)
(struct duration [n divisions] #:transparent)

;; duration=? : Duration Duration -> Bool
(define (duration=? a b)
  (match* [a b]
    [[(duration an ad) (duration bn bd)]
     (= (/ an ad) (/ bn bd))]))

;; duration<? : Duration Duration -> Bool
(define (duration<? a b)
  (match* [a b]
    [[(duration an ad) (duration bn bd)]
     (< (/ an ad) (/ bn bd))]))

;; duration<=? : Duration Duration -> Bool
(define (duration<=? a b)
  (match* [a b]
    [[(duration an ad) (duration bn bd)]
     (<= (/ an ad) (/ bn bd))]))

;; duration-common-divisions : Duration ... -> PosInt
(define (duration-common-divisions . ds)
  (apply lcm (map duration-divisions ds)))

;; duration-n/divisions : Duration PosInt -> Nat
(define (duration-n/divisions d divisions)
  (match d
    [(duration dn dd)
     (unless (= (lcm dd divisions) divisions)
       (error 'duration-n/divisions "not divisible"))
     (* dn (/ divisions dd))]))

;; duration+ : Duration Duration -> Duration
(define (duration+ a b)
  (define d (duration-common-divisions a b))
  (duration
   (+ (duration-n/divisions a d)
      (duration-n/divisions b d))
   d))

;; duration∆ : Duration Duration -> Duration
(define (duration∆ a b)
  (define d (duration-common-divisions a b))
  (duration
   (- (duration-n/divisions b d)
      (duration-n/divisions a d))
   d))

;; duration-divide : Duration Duration -> (values Nat Duration)
(define (duration-divide a b)
  (define d (duration-common-divisions a b))
  (define an (duration-n/divisions a d))
  (define bn (duration-n/divisions b d))
  (define base (gcd an bn))
  (define n (/ an base))
  (values n (duration base d)))

;; duration-fraction : Duration Duration -> ExactRational
(define (duration-fraction a b)
  (match* [a b]
    [[(duration an ad) (duration bn bd)]
     (/ (/ an ad) (/ bn bd))]))

(define duration-zero (duration 0 1))
(define duration-quarter (duration 1 1))
(define duration-eighth (duration 1 2))
(define duration-sixteenth (duration 1 4))

(define duration-half (duration 2 1))
(define duration-whole (duration 4 1))

(define duration-dotted-quarter
  (duration+ duration-quarter duration-eighth))
(define duration-dotted-eighth
  (duration+ duration-eighth duration-sixteenth))

(define duration-dotted-half
  (duration+ duration-half duration-quarter))
(define duration-dotted-whole
  (duration+ duration-whole duration-half))

;; ------------------------------------------------------------------------

