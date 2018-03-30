#lang agile

(require graph
         "../time/main.rkt"
         "../score/key-signature.rkt"
         "partition-point.rkt"
         "infer-segment-chord.rkt")

;; A [WeightedDirectedGraph V] is a weighted directed graph with
;; vertices of type V

;; ------------------------------------------------------------------------

;; Segmentation as Graph Search

;; Based on the `Segmentation as Graph Search` section of Algorithms for
;; Chordal Analysis

;; --------------------------------------------------------------------
;; | 1. Create one vertex for each partition point.                   |
;; | 2. Sort the vertices according to partition-point number.        |
;; | 3. Make an edge between each vertex, i and every other vertex j, |
;; |    where i<j.                                                    |
;; | 4. Make the reward (weight) for each edge i,j equal to the score |
;; |    of the segment from partition point i to partition point j.   |
;; --------------------------------------------------------------------

;; ------------------------------------------------------------------------

(provide infer-segmentation)

;; infer-segmentation :
;; [Listof NoteThere] Key -> [Listof [Timed [Maybe ChordSymbol]]]
(define (infer-segmentation nts key)
  (define-values [graph start end edge-chords]
    (segmentation-problem->graph-problem nts key))
  (define-values [vertex-distances vertex-predecessors]
    (bellman-ford graph start))
  (define path
    (let loop ([path '()] [v end])
      (cond [(equal? v start) (cons v path)]
            [else (loop (cons v path) (hash-ref vertex-predecessors v))])))
  (cond
    [(empty? path) '()]
    [(empty? (rest path)) '()]
    [else
     (let loop ([chords '()]
                [a (first path)]
                [b (second path)]
                [rst (rest (rest path))])
       (define ab-chord (hash-ref edge-chords (list a b)))
       (cond [(empty? rst) (reverse (cons ab-chord chords))]
             [else (loop (cons ab-chord chords)
                         b
                         (first rst)
                         (rest rst))]))]))

;; ------------------------------------------------------------------------

;; segmentation-problem->graph-problem :
;; [Listof NoteThere]
;; Key
;; ->
;; (values
;;   [WeightedDirectedGraph PartitionPoint]
;;   PartitionPoint
;;   PartitionPoint
;;   [Hashof [List PartitionPoint PartitionPoint] [Maybe ChordSymbol]])

;; I'm making the weights the negation of the scores so that I can use the
;; shortest-path algorithm (bellman-ford) in the generic graph library.
;; This should be fine since there are no cycles.
(define (segmentation-problem->graph-problem nts key)
  (define pps (partition-points nts))
  ;; TODO: How do you use the edge-chords hash table?
  (define-values [weights/edges edge-chords]
    (for/fold ([weights/edges '()]
               [edge-chords (hash)])
              ([a/b (in-combinations pps 2)])
      (match-define (list a b) a/b)
      (define seg (time-period a (position∆ a b)))
      (define nws (note-weights/minimal-segments seg nts))
      (define chord-sym (analyze-chord/note-weights seg nws key))
      (define score ((score-chord-template nws) chord-sym))
      (values
       (cons (list (- 4 score) a b) weights/edges)
       (hash-set edge-chords (list a b) (timed seg chord-sym)))))
  (values
   (weighted-graph/directed weights/edges)
   (if (empty? pps) #f (first pps))
   (if (empty? pps) #f (last pps))
   edge-chords))

;; ------------------------------------------------------------------------

