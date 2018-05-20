#lang agile

(require musicxml/note
         (prefix-in data/
           (combine-in
            music/data/note/main)))
(module+ test
  (require rackunit
           (submod music/data/note/note example)))

;; ---------------------------------------------------------

(provide pitch step alter octave)

;; ---------------------------------------------------------

(provide note->musicxml-pitch)

;; note->musicxml-pitch : Note -> MXexpr
(define (note->musicxml-pitch n)
  (define alteration (data/note-alteration n))
  (cond
    [(zero? alteration)
     (pitch '()
       (list
        (step '() (list (data/note-name-string n)))
        (octave '() (list (number->string (data/note-octave n))))))]
    [else
     (pitch '()
       (list
        (step '() (list (data/note-name-string n)))
        (alter '() (list (number->string alteration)))
        (octave '() (list (number->string (data/note-octave n))))))]))

;; ---------------------------------------------------------

(module+ test
  (check-equal? (note->musicxml-pitch C4)
                (pitch '()
                  (list
                   (step '() '("C"))
                   (octave '() '("4")))))

  (check-equal? (note->musicxml-pitch E♭5)
                (pitch '()
                  (list
                   (step '() '("E"))
                   (alter '() '("-1"))
                   (octave '() '("5")))))

  (check-equal? (note->musicxml-pitch F#3)
                (pitch '()
                  (list
                   (step '() '("F"))
                   (alter '() '("1"))
                   (octave '() '("3")))))
  )

