#lang racket/base

(provide which)

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
