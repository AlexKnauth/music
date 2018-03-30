#lang agile

(provide txexpr
         leaf/str
         leaf/num
         (all-from-out (submod txexpr safe)))

(require (rename-in (submod txexpr safe) [txexpr -txexpr]))

;; ------------------------------------------------------------------------

(define-match-expander txexpr
  (syntax-parser
    [(_ tag-pat:expr attrs-pat:expr elements-pat:expr)
     #'(? txexpr? (app txexpr->values tag-pat attrs-pat elements-pat))])
  (syntax-parser
    [:id #'-txexpr]
    [(_ tag:expr attrs:expr elements:expr)
     #'(-txexpr tag attrs elements)]))

(define-match-expander leaf/str
  (syntax-parser
    [(_ str-pat:expr)
     #'(? listof-string? (app str-leaf str-pat))]))

(define-match-expander leaf/num
  (syntax-parser
    [(_ num-pat:expr)
     #'(? listof-string? (app num-leaf (? number? num-pat)))]))

;; ------------------------------------------------------------------------

;; listof-string? : Any -> Bool
(define (listof-string? v)
  (and (list? v) (andmap string? v)))

;; str-leaf : [Listof String] -> String
(define (str-leaf elements)
  (apply string-append elements))

;; num-leaf : [Listof String] -> [Maybe Number]
(define (num-leaf elements)
  (string->number (str-leaf elements)))

;; ------------------------------------------------------------------------

