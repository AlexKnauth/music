#lang agile

(module+ test
  (require rackunit))

;; ------------------------------------------------------------------------

;; Note Classes (without octaves)

(provide note-class
         note-class=? note-class-midi=?
         note-class-alteration+ flat sharp
         note-class-alteration
         note-class->string
         name-class-string
         alteration-string)

;; A NoteClass is a (note-class PitchClass NameClass)
;; A MidiClass is an Int[0,12) representing one of the midi tones mod 12
;; A NameClass is an Int[0,7) representing which name it belongs to mod 12
(struct note-class [midi name] #:transparent)

;; note-class=? : NoteClass NoteClass -> Bool
(define (note-class=? a b)
  (and (= (note-class-midi a) (note-class-midi b))
       (= (note-class-name a) (note-class-name b))))

;; note-class-midi=? : NoteClass NoteClass -> Bool
(define (note-class-midi=? a b)
  (= (note-class-midi a) (note-class-midi b)))

;; note-class-alteration+ : NoteClass Int -> NoteClass
(define (note-class-alteration+ nc i)
  (match nc
    [(note-class midi name)
     (note-class (modulo (+ midi i) 12) name)]))

;; sharp : NoteClass -> NoteClass
(define (sharp nc) (note-class-alteration+ nc +1))

;; flat : NoteClass -> NoteClass
(define (flat nc) (note-class-alteration+ nc -1))

;; note-class-alteration : NoteClass -> Int
(define (note-class-alteration nc)
  (define (closest a)
    (let ([a (modulo a 12)])
      (argmin abs (list a (- a 12)))))
  (match nc
    [(note-class midi name)
     (match name
       [0 (closest (- midi 0))]
       [1 (closest (- midi 2))]
       [2 (closest (- midi 4))]
       [3 (closest (- midi 5))]
       [4 (closest (- midi 7))]
       [5 (closest (- midi 9))]
       [6 (closest (- midi 11))])]))

;; note-class->string : NoteClass -> String
(define (note-class->string nc)
  (string-append
   (name-class-string (note-class-name nc))
   (alteration-string (note-class-alteration nc))))

;; name-class-string : NameClass -> String
(define (name-class-string nc)
  (match nc
    [0 "C"]
    [1 "D"]
    [2 "E"]
    [3 "F"]
    [4 "G"]
    [5 "A"]
    [6 "B"]))

;; alteration-string : Int -> String
(define (alteration-string a)
  (match a
    [0 ""]
    [1 "#"]
    [-1 "♭"]
    [2 "𝄪"]
    [-2 "𝄫"]))

;; ------------------------------------------------------------------------

(provide C D E F G A B
         F# C# G# D# A# E# B#
         B♭ E♭ A♭ D♭ G♭ C♭ F♭)

(define C (note-class 0 0))
(define D (note-class 2 1))
(define E (note-class 4 2))
(define F (note-class 5 3))
(define G (note-class 7 4))
(define A (note-class 9 5))
(define B (note-class 11 6))

(define F# (sharp F))
(define C# (sharp C))
(define G# (sharp G))
(define D# (sharp D))
(define A# (sharp A))
(define E# (sharp E))
(define B# (sharp B))

(define B♭ (flat B))
(define E♭ (flat E))
(define A♭ (flat A))
(define D♭ (flat D))
(define G♭ (flat G))
(define C♭ (flat C))
(define F♭ (flat F))

(module+ test
  (define-check (check-note-class-string nc str)
    (check-equal? (note-class->string nc) str))

  (define-simple-macro (check-note-class-strings nc:id ...)
    #:with [chk ...]
    (for/list ([nc (in-list (syntax->list #'[nc ...]))])
      (quasisyntax/loc nc
        (check-note-class-string #,nc '#,(symbol->string (syntax-e nc)))))
    (begin chk ...))

  (check-note-class-strings
   C D E F G A B
   F# C# G# D# A#
   B♭ E♭ A♭ D♭ G♭))

;; ------------------------------------------------------------------------

