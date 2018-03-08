#lang agile

(require (submod txexpr safe)
         (prefix-in data/
           (combine-in
            music-theory/data/score/clef)))

;; ------------------------------------------------------------------------

(provide clef)

(define (clef #:sign sign-str #:line line-str #:octave octave-str)
  (txexpr 'clef '()
    (list*
     (txexpr 'sign '() (list sign-str))
     (txexpr 'line '() (list line-str))
     (if (string=? octave-str "0")
         '()
         (list
          (txexpr 'clef-octave-change '() (list octave-str)))))))

;; ------------------------------------------------------------------------

(provide clef->attribute-musicxml)

(define (clef->attribute-musicxml c)
  (match c
    [(data/clef ct oct)
     (define-values [sign line]
       (data/clef-type->sign+line ct))
     (clef #:sign sign
           #:line (number->string line)
           #:octave (number->string oct))]))

;; ------------------------------------------------------------------------

