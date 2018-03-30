#lang racket/base

(provide filter-maximal)

(module+ test
  (require rackunit
           (only-in racket/list in-permutations)
           (only-in racket/set set=? subset?)))

;; filter-maximal : [Listof X] [Y Y -> Bool] [X -> Y] -> [Listof X]
;; <? is a partial ordering predicate
(define (filter-maximal xs <? key)
  (reverse
   (for/fold ([acc '()])
             ([x (in-list xs)])
     (merge-with x acc <? key))))

;; merge-with : X [Listof X] [Y Y -> Bool] [X -> Y] -> [Listof X]
;; <? is a partial ordering predicate
(define (merge-with x ys <? key)
  (define xk (key x))
  (define (greater? y) (<? xk (key y)))
  (cond [(ormap greater? ys) ys]
        [else
         (define (not-lesser? y) (not (<? (key y) xk)))
         (cons x (filter not-lesser? ys))]))

;; ----------------------------------------------------------------------------

(module+ test
  (define-check (check-filter-maximal lst <? expected)
    (test-begin
     (for ([p (in-permutations lst)])
       (check set=? (filter-maximal p <? id) expected))))

  (define (id x) x)

  (check-equal? (filter-maximal '(1 2 3 2 3 2 1) < id) '(3 3))
  (check-equal? (filter-maximal '(1 2 3 2 3.0 2 1) < id) '(3 3.0))
  (check-equal? (filter-maximal '(1 2 3.0 2 3 2 1) < id) '(3.0 3))

  (check-equal? (filter-maximal '({} {a} {b} {c}) subset? id) '({a} {b} {c}))
  (check-equal? (filter-maximal '({b} {} {a} {c}) subset? id) '({b} {a} {c}))
  (check-equal? (filter-maximal '({c} {b} {a} {}) subset? id) '({c} {b} {a}))

  (check-filter-maximal '({} {a} {b}) subset? '({a} {b}))
  (check-filter-maximal '({} {a} {b} {a b}) subset? '({a b}))
  (check-filter-maximal '({} {a} {b} {c} {a b}) subset? '({a b} {c}))
  (check-filter-maximal '({} {a} {b} {c} {a b} {c a} {b c}) subset?
                        '({a b} {c a} {b c}))
  (check-filter-maximal '({} {a} {b} {c} {a b} {c a} {b c}) subset?
                        '({a b} {c a} {b c}))
  (check-filter-maximal '({} {a} {b} {c} {b c d} {a b} {c a} {b c}) subset?
                        '({a b} {c a} {b c d}))
  (check-filter-maximal '({} {a} {b} {c} {a b c d} {a b} {c a} {b c}) subset?
                        '({a b c d}))
  )
