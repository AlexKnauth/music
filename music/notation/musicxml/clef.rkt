#lang agile

(require musicxml/clef
         (prefix-in data/
           (combine-in
            music/data/score/clef)))

;; ------------------------------------------------------------------------

(provide clef sign line clef-octave-change)

;; ------------------------------------------------------------------------

(provide clef->attribute-musicxml)

(define (clef->attribute-musicxml c)
  (match c
    [(data/clef ct oct)
     (define-values [sgn ln]
       (data/clef-type->sign+line ct))
     (clef
      '()
      (list*
       (sign '() (list sgn))
       (line '() (list (number->string ln)))
       (if (zero? oct)
           '()
           (list (clef-octave-change '() (list (number->string oct)))))))]))

;; ------------------------------------------------------------------------

