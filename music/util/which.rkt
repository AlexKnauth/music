#lang racket/base

(provide which
         call-with-PATH)

(require racket/port
         racket/string
         racket/system)

;; which : String -> String
(define (which name)
  (string-trim
   (with-output-to-string
     (λ ()
       (unless (system (format "bash --login -c 'which ~v'" name))
         (error 'which "could not find ~v" name))))))

(define (PATH)
  (string-trim
   (with-output-to-string
     (λ ()
       (unless (system (format "bash --login -c 'echo $PATH'"))
         (error 'PATH))))))

(define (call-with-PATH f)
  (parameterize ([current-environment-variables
                  (environment-variables-copy
                   (current-environment-variables))])
    (putenv "PATH" (PATH))
    (f)))
