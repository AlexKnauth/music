#lang agile

(require racket/bool
         racket/format
         musicxml/metadata
         (prefix-in data/
           (combine-in
            music/data/score/metadata)))

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
       [(false? title) (list (work '() '()))]
       [else (list (work '() (list (work-title '() (list title)))))])]))

(define (movement-number->musicxml-elements mn)
  (match mn
    [#false '()]
    [n (list (movement-number '() (list (number->string n))))]))

(define (movement-title->musicxml-elements mt)
  (match mt
    [#false '()]
    [t (list (movement-title '() (list t)))]))

(define (identification->musicxml-elements c)
  (match c
    [#false '()]
    [(data/creator #false) '()]
    [(data/creator (? string? composer-str))
     (list
      (identification '()
        (list (creator '([type "composer"]) (list composer-str)))))]))

;; ------------------------------------------------------------------------

