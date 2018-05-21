#lang agile

(require musicxml/forward-backup
         musicxml/duration
         (prefix-in data/
           (combine-in
            music/data/time/main)))
(module+ test
  (require rackunit))

;; ---------------------------------------------------------

(provide forward backup)

;; ---------------------------------------------------------

(provide forward-duration->musicxml
         backup-duration->musicxml)

;; Does not include voice properties. Those will be added by
;; "score.rkt" if needed.

;; forward-duration->musicxml : Duration PosInt -> MXexpr
(define (forward-duration->musicxml d divisions)
  (define n (data/duration-n/divisions d divisions))
  (forward '() (list (duration '() (list (number->string n))))))

;; backup-duration->musicxml : Duration PosInt -> MXexpr
(define (backup-duration->musicxml d divisions)
  (define n (data/duration-n/divisions d divisions))
  (backup '() (list (duration '() (list (number->string n))))))

;; ---------------------------------------------------------

