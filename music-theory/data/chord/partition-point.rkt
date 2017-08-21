#lang agile

(provide partition-points
         partition-points/segment
         time-period-count-minimal-segments)

(require "../time/main.rkt")

;; A PartitionPoint is a Position
;; where a note (or other timed element) begins or ends

;; A Segment is a TimePeriod
;; that begins at a partition point and ends at a partition point

;; partition-points : [Listof [Timed Any]] -> [Listof PartitionPoint]
;; Do not account for measure length
;; (time-period-count-minimal-segments doesn't)
(define (partition-points tes)
  (remove-duplicates
   (sorted/position
    (for/list ([te (in-list tes)])
      (match te
        [(timed tp _)
         (list (time-period-start tp) (time-period-end tp))])))))

;; partition-points/segment :
;; Segment [Listof [Timed Any]] -> [Listof PartitionPoint]
(define (partition-points/segment seg tes)
  (remove-duplicates
   (sorted/position
    (for/list ([te (in-list tes)])
      (match te
        [(timed tp _)
         (define a (time-period-start tp))
         (define b (time-period-end tp))
         (cond
           [(and (time-period-contains-pos? seg a)
                 (time-period-contains-pos? seg b))
            (list a b)]
           [(time-period-contains-pos? seg a)
            (list a)]
           [(time-period-contains-pos? seg b)
            (list b)]
           [else
            (list)])])))))

;; time-period-count-minimal-segments :
;; TimePeriod [Listof PartitionPoint] -> Nat
;; Do not account for measure length
;; (partition-points doesn't)
(define (time-period-count-minimal-segments time-per part-points)
  (for/sum ([pos (in-list part-points)]
            #:when (time-period-contains-pos? time-per pos))
    1))

