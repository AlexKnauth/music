#lang racket/base

(provide clef->image)

(require racket/match
         2htdp/image
         lang/posn
         music/data/score/main)

(define (+line img p1 p2 color)
  (add-line img
            (posn-x p1) (posn-y p1)
            (posn-x p2) (posn-y p2)
            color))

(define (+curve img p1 a1 u1 p2 a2 u2 color)
  (add-curve img
             (posn-x p1) (posn-y p1) a1 u1
             (posn-x p2) (posn-y p2) a2 u2
             color))

;; --------------------------------------------------------------

(define STAFF-NUM-LINES 5)
(define STAFF-NUM-SPACES (sub1 STAFF-NUM-LINES))

;; --------------------------------------------------------------

;; puts the pinhole on the top line of the staff
(define (clef->image space-height color c)
  (define staff-height (* STAFF-NUM-SPACES space-height))
  (define W (* 1/2 staff-height))
  (define H (* 3/2 staff-height))
  (define CTR-X (* 1/2 W))
  (define CTR-Y (* 1/2 H))
  (define MT (rectangle W H "solid" "transparent"))
  (define staff-y (* 1 space-height))
  (match c
    [(== TREBLE-CLEF)
     (let*
       ([main-line-top
         (make-posn CTR-X 0)]
        [main-line-bot
         (make-posn CTR-X (+ staff-y staff-height (* 1/2 space-height)))]
        [tail-end
         (make-posn (+ CTR-X (* -3/4 space-height))
                    (+ staff-y staff-height (* 1/2 space-height)))]
        [curve-cross-mid
         (make-posn CTR-X (+ staff-y space-height))]
        [curve-cross-bot
         (make-posn CTR-X (+ staff-y staff-height))]
        [spiral-cross-top
         (make-posn CTR-X (+ staff-y (* 2 space-height)))]
        [spiral-end
         (make-posn CTR-X (+ staff-y (* (+ 3 1/4) space-height)))]
  
        [img MT]
        ;; the main line
        [img (+line img main-line-top main-line-bot color)]
        ;; the curved tail off the main line on the left
        [img (+curve img
                     main-line-bot 270 1/2
                     tail-end       90 1/2
                     color)]
        ;; from the top, going down on the right
        [img (+curve img
                     main-line-top   0          1/4
                     curve-cross-mid (+ 180 30) 1/2
                     color)]
        ;; further down on the left
        [img (+curve img
                     curve-cross-mid (+ 180 30) 1/2
                     curve-cross-bot 0          1/2
                     color)]
        ;; start spiraling on the right
        [img (+curve img
                     curve-cross-bot    0 1/2
                     spiral-cross-top 180 1/2
                     color)]
        ;; end of the spiral on the left
        [img (+curve img
                     spiral-cross-top 180 1/2
                     spiral-end         0 1/2
                     color)])
       (put-pinhole CTR-X staff-y img))]))

(module+ test
  (clear-pinhole (clef->image 10 "black" TREBLE-CLEF))
  )
