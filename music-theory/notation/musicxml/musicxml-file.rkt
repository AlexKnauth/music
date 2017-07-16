#lang at-exp agile

(require racket/path
         racket/system
         (submod txexpr safe)
         (only-in xml xexpr->string write-xexpr))

(define (str . strs)
  (string->immutable-string (apply string-append strs)))

;; A MXexpr is a TXexpr in MusicXML format

;; ------------------------------------------------------------------------

(provide write-musicxml-file write-musicxml)

;; write-musicxml-file : PathString MXexpr -> Void
(define (write-musicxml-file file-path mx #:exists [exists 'error])
  (call-with-output-file* file-path
    (λ (out) (write-musicxml mx out))
    #:exists exists))

;; write-musicxml : MXexpr OutputPort -> Void
(define (write-musicxml mx out)
  (write-string XML-declaration out)
  (newline out)
  (write-string MusicXML-DOCTYPE-declaration out)
  (newline out)
  (write-xexpr mx out))

(define XML-declaration
  @str{<?xml version="1.0" encoding="UTF-8" standalone="no"?>})

(define MusicXML-DOCTYPE-declaration
  @str{<!DOCTYPE score-partwise PUBLIC
           "-//Recordare//DTD MusicXML 3.0 Partwise//EN"
           "http://www.musicxml.org/dtds/partwise.dtd">})

;; ------------------------------------------------------------------------

(provide open-musicxml-file/MuseScore-2)

(define (open-musicxml-file/MuseScore-2 file-path)
  (system (format "open -a ~v ~v"
                  "MuseScore 2"
                  (path->string (simple-form-path file-path)))))

;; ------------------------------------------------------------------------

