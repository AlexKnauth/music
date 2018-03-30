#lang agile

(provide define-product-combinations)

(require (for-syntax racket/base
                       racket/list
                       racket/syntax
                       syntax/stx))

(define-simple-macro
  (define-product-combinations combine:id
    [[x:id e:expr] ...]
    ...)
  #:with [x* ...]
  (for/list ([xs (in-list (apply cartesian-product
                                 (stx-map stx->list #'[[x ...] ...])))])
    (format-id (first xs) "~a"
               (apply string-append (map symbol->string (map syntax-e xs)))
               #:source (first xs)
               #:props (first xs)))
  #:with [e* ...]
  (for/list ([es (in-list (apply cartesian-product
                                 (stx-map stx->list #'[[e ...] ...])))])
    #`(combine #,@es))
  (begin
    (define x* e*)
    ...))

