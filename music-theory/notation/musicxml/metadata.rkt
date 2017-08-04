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

;; ------------------------------------------------------------------------

(provide metadata->musicxml-elements)

;; metadata->musicxml-elements : Metadata -> [Listof MXexpr]
(define (metadata->musicxml-elements m)
  (match m
    [#false '()]
    [(data/metadata work)
     (cond [(false? work) '()]
           [else (list (work->musicxml work))])]))

;; work->musicxml : Work -> MXexpr
(define (work->musicxml w)
  (match w
    [(data/work title)
     (cond
       [(false? title) (work)]
       [else (work (work-title title))])]))

;; ------------------------------------------------------------------------

