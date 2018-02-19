#lang agile

(provide (struct-out voiced)
         assign-voices)

(require graph
         music-theory/data/time/position
         music-theory/data/time/time-period)

;; ------------------------------------------------------------------------

;; A [Voiced X] is a (voiced Nat X)
(struct voiced [voice value] #:transparent)

;; ------------------------------------------------------------------------

;; assign-voices :
;;   [Listof [Timed X]]
;;   [[Listof [Timed X]] -> [Listof [Timed X]]]
;;   ->
;;   [Listof [Voiced [Listof [Timed X]]]]
(define (assign-voices txs order)
  ;; an "interval graph" containing edges for every
  ;; time-period conflict
  (define G
    (unweighted-graph/undirected
     (for/list ([p (in-combinations txs 2)]
                #:when (timed-overlap? (first p) (second p)))
       p)))

  ;; n   : Nat
  ;; col : [Hashof [Timed X] Nat]
  (define-values [n col] (my-coloring G order))

  (define voices
    (for/fold ([vcs (hash)])
              ([tx (in-list txs)])
      ;; If tx isn't "in" the graph, it has no edges, which
      ;; means it's safe to assign any voice to it. Here, if
      ;; it's not "in" the graph, it's assigned voice zero.
      (hash-update vcs
                   (hash-ref col tx 0)
        (λ (old) (cons tx old))
        (λ () '()))))

  (for/list ([vc (in-range (max 1 n))])
    (voiced vc (reverse (hash-ref voices vc)))))

;; G must be an "interval graph"
;; (https://en.wikipedia.org/wiki/Interval_graph)
;; TODO: Are there particular coloring algorithms that work
;;       really well for interval graphs?
;;       Is ordering them by `timed-end` helping the way I
;;       intend it to?
(define (my-coloring G order)
  (coloring/greedy G #:order order))

;; ------------------------------------------------------------------------

