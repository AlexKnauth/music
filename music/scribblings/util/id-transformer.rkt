#lang racket/base

(provide id-transformer)

(require syntax/stx)

;; id-transformer : [Identifier -> Syntax] -> [Syntax -> Syntax]
(define ((id-transformer trans) stx)
  (cond
    [(identifier? stx) (trans stx)]
    [(and (stx-pair? stx) (identifier? (stx-car stx)))
     (datum->syntax stx
                    (cons (trans (stx-car stx))
                          (stx-cdr stx))
                    stx
                    stx)]
    [else
     (raise-syntax-error #f "bad use of identifier macro" stx)]))

