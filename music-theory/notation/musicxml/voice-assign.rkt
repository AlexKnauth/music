#lang agile

(provide assign-voices)

(require graph
         music-theory/data/time/position
         music-theory/data/time/time-period)

;; ------------------------------------------------------------------------

;; assign-voices : [Listof [Timed X]] -> [Listof [List [Timed X] Nat]]
(define (assign-voices txs)
  ;; an "interval graph" containing edges for every
  ;; time-period conflict
  (define G
    (unweighted-graph/undirected
     (for/list ([p (in-combinations txs 2)]
                #:when (timed-overlap? (first p) (second p)))
       p)))

  ;; col : [Hashof [Timed X] Nat]
  (define col (my-coloring G))

  (for/list ([tx (in-list txs)])
    ;; If tx isn't "in" the graph, it has no edges, which
    ;; means it's safe to assign any voice to it. Here, if
    ;; it's not "in" the graph, it's assigned voice zero.
    (list tx (hash-ref col tx 0))))

;; G must be an "interval graph"
;; (https://en.wikipedia.org/wiki/Interval_graph)
;; TODO: Are there particular coloring algorithms that work
;;       really well for interval graphs?
;;       Is ordering them by `timed-end` helping the way I
;;       intend it to?
(define (my-coloring G)
  (define (order Vs)
    (sort Vs position<? #:key timed-end))
  (let-values ([(n c) (coloring/greedy G #:order order)])
    c))

;; ------------------------------------------------------------------------

