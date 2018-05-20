#lang agile

(require musicxml/time-signature
         (prefix-in data/
           (combine-in
            music/data/time/main
            music/data/time/time-signature)))

;; ---------------------------------------------------------

(provide time beats beat-type)

;; ---------------------------------------------------------

(provide time->attribute-musicxml)

;; time->attribute-musicxml : TimeSig -> MXexpr
(define (time->attribute-musicxml ts)
  (define-values [bs bt]
    (data/time-sig->nd-values ts))
  (time '()
    (list
     (beats '() (list (number->string bs)))
     (beat-type '()
       (list
        (cond
          [(data/duration=? bt data/duration-whole) "1"]
          [(data/duration=? bt data/duration-half) "2"]
          [(data/duration=? bt data/duration-quarter) "4"]
          [(data/duration=? bt data/duration-eighth) "8"]
          [(data/duration=? bt data/duration-sixteenth) "16"]
          [else (error 'beat-type->musicxml "given beat-type: ~v" bt)]))))))

