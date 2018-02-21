#lang racket/base

(provide dd)

(require syntax/parse/define
         (only-in scribble/manual racket)
         (for-syntax racket/base))

(define-syntax-parser dd
  [(_ name:id) #'name]
  [(_ [name:id d:expr ...]) #'(racket [#,name #,(dd d) ...])])

