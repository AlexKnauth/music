#lang agile

(require musicxml/music-data
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

(provide forward-duration-divisions
         backup-duration-divisions)

;; private
;; Any -> Boolean
(define (duration? v)
  (match v [(duration _ _) #true] [_ #false]))

;; Forward -> PositiveDivisions
(define (forward-duration-divisions n)
  (match n
    [(forward _ (list _ ... (? duration? d) _ ...))
     (duration-divisions d)]))

;; Backup -> PositiveDivisions
(define (backup-duration-divisions n)
  (match n
    [(backup _ (list _ ... (? duration? d) _ ...))
     (duration-divisions d)]))

;; ---------------------------------------------------------

