#lang agile

(provide defs defs/f)

(define-simple-macro (defs [x:id e:expr] ...)
  (begin (define x e) ...))
(define-simple-macro (defs/f f:id [x:id e:expr] ...)
  (defs [x (f e)] ...))

