#lang agile

;; ------------------------------------------------------------------------

(provide metadata work)

;; A MetaData is a (metedata [Maybe Work])
(struct metadata [work] #:transparent)

;; A Work is a (work [Maybe String])
(struct work [title] #:transparent)

;; ------------------------------------------------------------------------

