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
         duration/2
         duration-divide
         duration-fraction
         duration-zero
         duration-quarter
         duration-eighth
         duration-sixteenth
         duration-16th
         duration-32nd
         duration-64th
         duration-128th
         duration-256th
         duration-512th
         duration-1024th
         duration-half
         duration-whole
         duration-double-whole
         duration-whole*2
         duration-quadruple-whole
         duration-whole*4
         duration-whole*8
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

;; duration/2 : Duration -> Duration
(define (duration/2 d)
  (match d
    [(duration dn dd)
     (if (even? dn)
         (duration (/ dn 2) dd)
         (duration dn (* 2 dd)))]))

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
(define duration-16th duration-sixteenth)
(define duration-32nd (duration 1 8))
(define duration-64th (duration 1 16))
(define duration-128th (duration 1 32))
(define duration-256th (duration 1 64))
(define duration-512th (duration 1 128))
(define duration-1024th (duration 1 1024))

(define duration-half (duration 2 1))
(define duration-whole (duration 4 1))
(define duration-double-whole (duration 8 1))
(define duration-whole*2 duration-double-whole)
(define duration-quadruple-whole (duration 16 1))
(define duration-whole*4 duration-quadruple-whole)
(define duration-whole*8 (duration 32 1))

(define duration-dotted-quarter
  (duration+ duration-quarter duration-eighth))
(define duration-dotted-eighth
  (duration+ duration-eighth duration-sixteenth))

(define duration-dotted-half
  (duration+ duration-half duration-quarter))
(define duration-dotted-whole
  (duration+ duration-whole duration-half))

;; ------------------------------------------------------------------------

