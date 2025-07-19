#lang agile

(require racket/runtime-path
         music/data/chord/main
         music/data/instrument/main
         "../example/Bach-Goldberg-Canone-alla-Quarta.rkt"
         "../notation/musicxml/musicxml-file.rkt"
         "../notation/musicxml/score.rkt"
         "../notation/lilypond/lilypond-file.rkt")

(define Bach-Goldberg-Canone-alla-Quarta/guitar-chords
  (score-add-guitar-part Bach-Goldberg-Canone-alla-Quarta))

;; ------------------------------------------------------------------------

(define-runtime-path Bach-Goldberg-Canone-alla-Quarta.xml
  "Bach-Goldberg-Canone-alla-Quarta.xml")

(write-musicxml-file Bach-Goldberg-Canone-alla-Quarta.xml
                     (score->musicxml
                      Bach-Goldberg-Canone-alla-Quarta/guitar-chords)
                     #:exists 'replace
                     #:indentation 'peek)

(open-musicxml-file/MuseScore-4 Bach-Goldberg-Canone-alla-Quarta.xml)

(open-musicxml-file/lilypond Bach-Goldberg-Canone-alla-Quarta.xml)

;; ------------------------------------------------------------------------

