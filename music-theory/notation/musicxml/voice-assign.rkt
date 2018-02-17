#lang agile

(provide assign-voices)

(require graph
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
;;       Would ordering them by `timed-end` help?
;; Current:
;; Try generic graph coloring with 4 colors, and if that
;; doesn't work, try a greedy version with an unbounded
;; number of colors. Why 4 to start? Because MuseScore and
;; Sibelius support 4 voices.
(define (my-coloring G)
  (or
   (coloring G 4)
   (let-values ([(n c) (coloring/greedy G)])
     c)))

;; ------------------------------------------------------------------------

