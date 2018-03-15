#lang racket/base

(require racket/match
         2htdp/image
         music/data/score/main
         music/data/note/main
         music/data/time/main
         music/data/time/measures
         (submod music/data/note/note private)
         "util/stretchable.rkt"
         (for-syntax racket/base))

(define-syntax ....
  (λ (stx) (syntax/loc stx (error '....))))

(define (duration-quarters dur)
  (match dur
    [(duration n d) (/ n d)]))

(define (note-space-y n)
  (/ (note-staff-y n) 2))

;; --------------------------------------------------------------

;; An Image is an image from 2htdp/image

;; An SImage is a [Stretchable Image]
;; empty-s-image : SImage
(define empty-s-image
  (stretchable 0 0 (λ (w h) (rectangle w h "solid" "transparent"))))

;; sabove : SImage SImage -> SImage
;; sbeside : SImage SImage -> SImage
(define sabove (stretchable-above above))
(define sbeside (stretchable-beside beside))

(define (sabove* imgs) (foldr sabove empty-s-image imgs))
(define (sbeside* imgs) (foldr sbeside empty-s-image imgs))

;; --------------------------------------------------------------

;; Constants

(define STAFF-NUM-LINES 5)
(define STAFF-NUM-SPACES (sub1 STAFF-NUM-LINES))

(define QUARTER-DUR-WIDTH 30)
(define START-X (* 1 QUARTER-DUR-WIDTH))

(define STAFF-SPACE-HEIGHT 10)
(define STAFF-HEIGHT (* STAFF-NUM-SPACES STAFF-SPACE-HEIGHT))

(define STAFF-COLOR "black")

(define NOTEHEAD-RADIUS (* 1/2 STAFF-SPACE-HEIGHT))
(define NOTE-COLOR "black")
(define NOTE-ERROR-COLOR "firebrick")

;; --------------------------------------------------------------

;; Variables that are constant per-measure

;; A MRC is a
;;   (meas-render-consts PosReal PosReal PosReal PosReal PosReal)
(struct meas-render-consts
  [w h start-x staff-y quarter-dur-width]
  #:transparent)

;; --------------------------------------------------------------

;; score->image : Score -> Image
(define (score->image s)
  (match s
    [(score _ (list p))
     (stretchable-render/min-size
      (sbeside* (part->images p)))]))

;; part->images : Part -> [Listof SImage]
(define (part->images p)
  (match p
    [(part _ elems)
     (elements->images elems)]))

;; --------------------------------------------------------------

;; A State is a (state)
(struct state [] #:transparent)

;; st/measure-boundary : State TimeSig -> State
(define (st/measure-boundary s ts)
  (match s
    [(state)
     (state)]))

;; --------------------------------------------------------------

;; elements->images : SortedMusElements -> [Listof SImage]
(define (elements->images elems)
  (define measures (group-measures elems))
  (define init-st (state))
  (measures->images init-st measures))

;; measures->images : State [Listof Measure] -> [Listof SImage]
(define (measures->images st ms)
  (match ms
    ['()
     ;; TODO: deal with leftover state that might imply
     ;; more notes to render
     '()]
    [(cons m rst)
     (define ts (measure-time-sig m))
     (define-values [st* m-img]
       (measure->image st m))
     (cons
      m-img
      (measures->images (st/measure-boundary st* ts) rst))]))

;; measure->image : State Measure -> (values State SImage)
(define (measure->image st m)
  (match-define (measure ts elems) m)
  (define meas-dur (time-sig-measure-length ts))
  (define start-x START-X)
  (define quarters (duration-quarters meas-dur))
  (define W (+ start-x (* QUARTER-DUR-WIDTH quarters)))
  (define H (+ STAFF-HEIGHT (* 4 STAFF-SPACE-HEIGHT)))
  (define (F w h)
    (define quarter-dur-width (/ (- w start-x) quarters))
    (define staff-y           (/ (- h STAFF-HEIGHT) 2))
    (define mrc (meas-render-consts w h start-x staff-y quarter-dur-width))
    (place-elems
     mrc elems
     (place-barlines
      mrc
      (place-staff
       mrc
       (rectangle w h "outline" "transparent")))))
  (values
   (state)
   (stretchable W H F)))

;; --------------------------------------------------------------

(define (place-staff mrc img)
  (match-define (meas-render-consts w h start-x staff-y quarter-dur-width) mrc)
  (for/fold ([img img])
            ([i (in-range 0 STAFF-NUM-LINES)])
    (define y (+ staff-y (* i STAFF-SPACE-HEIGHT)))
    (add-line
     img
     0 y w y
     STAFF-COLOR)))

(define (place-barlines mrc img)
  (match-define (meas-render-consts w h start-x staff-y quarter-dur-width) mrc)
  (for/fold ([img img])
            ([x (in-list (list 0 w))])
    (add-line
     img
     x staff-y
     x (+ staff-y STAFF-HEIGHT)
     STAFF-COLOR)))

(define (place-elems mrc elems img)
  (match-define (meas-render-consts w h start-x staff-y quarter-dur-width) mrc)
  (for/fold ([img img])
            ([elem (in-list elems)])
    (place-elem mrc elem img)))

(define (place-elem mrc elem img)
  (match-define (meas-render-consts w h start-x staff-y quarter-dur-width) mrc)
  (match-define (timed tp e) elem)
  (match-define (time-period (position _ pos-dur) dur) tp)
  (define pos-d (duration-quarters pos-dur))
  (define x (+ start-x (* pos-d quarter-dur-width)))
  (cond
    [(note? e)
     (define F5 (F 5))
     (define y (+ staff-y (* (- (note-space-y F5)
                                (note-space-y e))
                             STAFF-SPACE-HEIGHT)))
     (place-image
      (render-notehead dur)
      x y
      img)]
    [else
     img]))

(define (render-notehead dur)
  (cond
    [(duration=? dur duration-quarter)
     (circle NOTEHEAD-RADIUS "solid" NOTE-COLOR)]
    [(duration=? dur duration-half)
     (circle NOTEHEAD-RADIUS "outline" NOTE-COLOR)]
    [else
     (circle NOTEHEAD-RADIUS "solid" NOTE-ERROR-COLOR)]))

;; --------------------------------------------------------------

(module+ test
  (define score-C5
    (score
     #f
     (list
      (part "Piano"
            (list
             (timed (time-period (position 0 beat-one) duration-zero)
                    (time-sig/nd 4 duration-quarter))
             (timed (time-period (position 0 beat-one) duration-quarter)
                    (C 5)))))))
  (define score-CDEG
    (score
     #f
     (list
      (part "Piano"
            (list
             (timed (time-period (position 0 beat-one) duration-zero)
                    (time-sig/nd 4 duration-quarter))
             (timed (time-period (position 0 beat-one) duration-quarter)
                    (C 4))
             (timed (time-period (position 0 beat-two) duration-quarter)
                    (D 4))
             (timed (time-period (position 0 beat-three) duration-quarter)
                    (E 4))
             (timed (time-period (position 0 beat-four) duration-quarter)
                    (G 4))
             (timed (time-period (position 1 beat-one) duration-quarter)
                    (F 4))
             (timed (time-period (position 1 beat-two) duration-quarter)
                    (A 4))
             (timed (time-period (position 1 beat-three) duration-half)
                    (G 4))
             )))))
  (score->image score-C5)
  (score->image score-CDEG)
  )

;; --------------------------------------------------------------

