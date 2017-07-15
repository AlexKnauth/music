#lang agile

(require "note.rkt")
(module+ example
  (provide (all-defined-out))
  (require (submod "note.rkt" example)
           (for-syntax racket/base
                       racket/syntax))
  (define-simple-macro
    (define-note-held-combinations
      #:notes
      [[n:id note:expr] ...]
      #:durations
      [[d:id duration:expr] ...])
    #:with [nd ...]
    (for*/list ([n (in-list (syntax->list #'[n ...]))]
                [d (in-list (syntax->list #'[d ...]))])
      (format-id n "~a~a" n d #:source n))
    #:with [noteduration ...]
    (for*/list ([note (in-list (syntax->list #'[note ...]))]
                [duration (in-list (syntax->list #'[duration ...]))])
      #`(note-held #,note #,duration))
    (begin
      (define nd noteduration)
      ...)))

;; ------------------------------------------------------------------------

(provide note-held
         note-held-note
         note-held-duration)

;; A NoteHeld is a (note-held Note Duration)
(struct note-held [note duration] #:transparent)

(module+ example
  (define-note-held-combinations
    #:notes
    [[C2 C2] [C3 C3] [C4 C4] [C5 C5]
     [D2 D2] [D3 D3] [D4 D4] [D5 D5]
     [E2 E2] [E3 E3] [E4 E4] [E5 E5]
     [F2 F2] [F3 F3] [F4 F4] [F5 F5]
     [G2 G2] [G3 G3] [G4 G4] [G5 G5]
     [A2 A2] [A3 A3] [A4 A4] [A5 A5]
     [B2 B2] [B3 B3] [B4 B4] [B5 B5]
     [F#4 F#4] [F#5 F#5]
     [C#4 C#4] [C#5 C#5]]
    #:durations
    [[𝅝 duration-whole]     [𝅝. duration-dotted-whole]
     [𝅗𝅥 duration-half]      [𝅗𝅥. duration-dotted-half]
     [♩ duration-quarter]   [♩. duration-dotted-quarter]
     [♪ duration-eighth]   [♪. duration-dotted-eighth]
     [𝅘𝅥𝅯 duration-sixteenth]])
  )

;; ------------------------------------------------------------------------

(provide duration
         duration-common-divisions
         duration-n/divisions
         duration=?
         duration<?
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

