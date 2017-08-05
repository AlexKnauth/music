#lang agile

;; ------------------------------------------------------------------------

(provide metadata work creator)

;; A MetaData is a
;; (metedata [Maybe Work]
;;           [Maybe Number]
;;           [Maybe String]
;;           [Maybe Creator])
(struct metadata
  [work
   movement-number
   movement-title
   composer]
  #:transparent)

;; A Work is a (work [Maybe String])
(struct work [title] #:transparent)

;; A Creator is a (creator [Maybe String])
(struct creator [composer] #:transparent)

;; ------------------------------------------------------------------------

