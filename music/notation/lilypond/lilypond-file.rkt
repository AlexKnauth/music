#lang agile

(require racket/file
         racket/path
         racket/system
         "../../util/which.rkt")

;; ------------------------------------------------------------------------

(provide open-musicxml-file/lilypond)

(define (open-musicxml-file/lilypond input)
  (define output (run-lilypond/musicxml input))
  (system (format "open ~v" (path->string output)))
  output)

;; ------------------------------------------------------------------------

(provide run-lilypond/musicxml)

(define (run-lilypond/musicxml input)
  (run-lilypond (run-musicxml2ly input)))

;; ------------------------------------------------------------------------

(provide run-lilypond
         run-musicxml2ly)

(define (run-lilypond input)
  (define output (simple-form-path (path-replace-extension input #".pdf")))
  (unless (bytes=? (path-get-extension output) #".pdf")
    (error 'run-lilypond "expected output target with extension `.pdf`"))
  (define lilypond (which "lilypond"))
  (println lilypond)
  (system* lilypond (simple-form-path input))
  output)

(define (run-musicxml2ly input)
  (define output (path-replace-extension input #".ly"))
  (define musicxml2ly (which "musicxml2ly"))
  (system* musicxml2ly
           (simple-form-path input))
  output)

;; ------------------------------------------------------------------------

(define (path-remove-extension path)
  (path-replace-extension path #""))

;; ------------------------------------------------------------------------

