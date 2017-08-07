#lang agile

(require (submod txexpr safe)
         (prefix-in data/
           (combine-in
            music-theory/data/score/clef)))

;; ------------------------------------------------------------------------

(provide clef)

(define (clef #:sign sign-str #:line line-str)
  (txexpr 'clef '()
    (list
     (txexpr 'sign '() (list sign-str))
     (txexpr 'line '() (list line-str)))))

;; ------------------------------------------------------------------------

(provide clef->attribute-musicxml)

(define (clef->attribute-musicxml c)
  (match c
    [(data/clef ct)
     (define-values [sign line]
       (data/clef-type->sign+line ct))
     (clef #:sign sign #:line (number->string line))]))

;; ------------------------------------------------------------------------

