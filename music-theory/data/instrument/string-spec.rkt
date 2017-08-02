#lang agile

(require "../note.rkt")

;; ------------------------------------------------------------------------

(provide violin-strings
         viola-strings
         cello-strings
         bass-strings
         guitar-strings)

;; A StringSpec is a [Listof Note]
;; Representing the notes that the open strings would play

(define violin-strings
  (list (G 3) (D 4) (A 4) (E 5)))

(define viola-strings
  (list (C 3) (G 3) (D 4) (A 4)))

(define cello-strings
  (list (C 2) (G 2) (D 3) (A 3)))

;; both string bass and bass guitar
(define bass-strings
  (list (E 1) (A 1) (D 2) (G 2)))

(define guitar-strings
  (list (E 2) (A 2) (D 3) (G 3) (B 3) (E 4)))

;; ------------------------------------------------------------------------

