#lang agile

(require racket/bool
         racket/format
         (submod txexpr safe)
         (prefix-in data/
           (combine-in
            "../../data/score/metadata.rkt")))

;; ------------------------------------------------------------------------

(define (work . elements)
  (txexpr 'work '() elements))

(define (work-title . elements)
  (txexpr 'work-title '() elements))

(define (movement-number . elements)
  (txexpr 'movement-number '() elements))

(define (movement-title . elements)
  (txexpr 'movement-title '() elements))

(define (identification . elements)
  (txexpr 'identification '() elements))

(define (creator #:type type . elements)
  (txexpr 'creator `([type ,type]) elements))

;; ------------------------------------------------------------------------

(provide metadata->musicxml-elements)

;; metadata->musicxml-elements : Metadata -> [Listof MXexpr]
(define (metadata->musicxml-elements m)
  (match m
    [#false '()]
    [(data/metadata work movement-number movement-title creator)
     (append
      (work->musicxml-elements work)
      (movement-number->musicxml-elements movement-number)
      (movement-title->musicxml-elements movement-title)
      (identification->musicxml-elements creator))]))

;; work->musicxml-elements : Work -> [Listof MXexpr]
(define (work->musicxml-elements w)
  (match w
    [#false '()]
    [(data/work title)
     (cond
       [(false? title) (list (work))]
       [else (list (work (work-title title)))])]))

(define (movement-number->musicxml-elements mn)
  (match mn
    [#false '()]
    [n (list (movement-number (number->string n)))]))

(define (movement-title->musicxml-elements mt)
  (match mt
    [#false '()]
    [t (list (movement-title t))]))

(define (identification->musicxml-elements c)
  (match c
    [#false '()]
    [(data/creator #false) '()]
    [(data/creator (? string? composer-str))
     (list (identification (creator #:type "composer" composer-str)))]))

;; ------------------------------------------------------------------------

