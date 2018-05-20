#lang agile

(require musicxml/key
         (prefix-in data/
           (combine-in
            music/data/score/key-signature)))

;; ---------------------------------------------------------

(provide key fifths)

;; ---------------------------------------------------------

(provide key->attribute-musicxml)

;; key->attribute-musicxml : Key -> MXexpr
(define (key->attribute-musicxml k)
  (key '()
    (list (fifths '()
            (list (number->string (data/key-fifths k)))))))

