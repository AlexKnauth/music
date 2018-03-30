#lang agile

(require racket/file
         racket/path
         racket/system)

;; ------------------------------------------------------------------------

(provide open-musicxml-file/lilypond)

(define (open-musicxml-file/lilypond input)
  (define output (simple-form-path (path-add-extension input #".pdf")))
  (run-lilypond/musicxml input output)
  (system (format "open ~v" (path->string output))))

;; ------------------------------------------------------------------------

(provide run-lilypond/musicxml)

(define (run-lilypond/musicxml input output)
  (define intermediate-ly (make-temporary-file "rkttmp~a.ly"))
  (run-musicxml2ly input intermediate-ly)
  (run-lilypond intermediate-ly output)
  (delete-file intermediate-ly))

;; ------------------------------------------------------------------------

(provide run-lilypond
         run-musicxml2ly)

(define (run-lilypond input output)
  (unless (bytes=? (path-get-extension output) #".pdf")
    (error 'run-lilypond "expected output target with extension `.pdf`"))
  (define output/no-ext (path-remove-extension output))
  (call-with-home-bin-PATH
   (λ ()
     (system (format "lilypond -o ~v ~v"
                     (path->string (simple-form-path output/no-ext))
                     (path->string (simple-form-path input)))))))

(define (run-musicxml2ly input output)
  (call-with-home-bin-PATH
   (λ ()
     (system (format "musicxml2ly -o ~v ~v"
                     (path->string (simple-form-path output))
                     (path->string (simple-form-path input)))))))

;; ------------------------------------------------------------------------

(define (call-with-home-bin-PATH proc)
  (define home-bin
    (path->string (build-path (find-system-path 'home-dir) "bin")))
  (when (regexp-match? (regexp-quote ":") home-bin)
    (error 'call-with-home-bin-PATH "this is probably bad"))
  (parameterize ([current-environment-variables
                  (environment-variables-copy
                   (current-environment-variables))])
    (putenv "PATH" (format "~a:~a" (getenv "PATH") home-bin))
    (proc)))

(define (path-remove-extension path)
  (path-replace-extension path #""))

;; ------------------------------------------------------------------------

