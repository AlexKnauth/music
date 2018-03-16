#lang racket/base

(require math/base
         racket/list
         racket/match
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
(define (above/pinhole a b)
  (above/align "pinhole" a b))
(define (beside/pinhole a b)
  (beside/align "pinhole" a b))
(define (place-image/pinhole img x y scene)
  (place-image/align img
                     x y
                     "pinhole" "pinhole"
                     scene))
(define (add-line/pinhole img x1 y1 x2 y2 pen/color)
  (define pre-x (min 0 x1 x2))
  (define pre-y (min 0 y1 y2))
  (define px (- (pinhole-x img) pre-x))
  (define py (- (pinhole-y img) pre-y))
  (put-pinhole
   px py
   (add-line img x1 y1 x2 y2 pen/color)))

;; An SImage is a [Stretchable Image]
;; empty-s-image : Nat Nat -> SImage
(define (empty-s-image wN hN)
  (stretchable (make-list wN 0)
               (make-list hN 0)
               (λ (w h) (center-pinhole empty-image))))

;; sabove : SImage SImage -> SImage
;; sbeside : SImage SImage -> SImage
(define sabove (stretchable-above above/pinhole))
(define sbeside (stretchable-beside beside/pinhole))

(define (sabove* imgs)
  (define MT
    (if (empty? imgs)
        (empty-s-image 0 0)
        (empty-s-image (length (stretchable-min-ws (first imgs))) 0)))
  (foldr sabove MT imgs))
(define (sbeside* imgs)
  (define MT
    (if (empty? imgs)
        (empty-s-image 0 0)
        (empty-s-image 0 (length (stretchable-min-hs (first imgs))))))
  (foldr sbeside MT imgs))

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

(define STEM-LENGTH (* (+ 3 1/2) STAFF-SPACE-HEIGHT))

;; --------------------------------------------------------------

(define TREBLE-TOP (F 5))
(define TREBLE-MID (B 4))
(define (treble-space-y n)
  (- (note-space-y TREBLE-TOP)
     (note-space-y n)))
(define (treble-stem-up? n)
  (<= (note-space-y n) (note-space-y TREBLE-MID)))

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
     (clear-pinhole
      (stretchable-render/min-size
       (sbeside* (part->images p))))]))

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
  (define space-ys (map treble-space-y (filter note? (map timed-value elems))))
  (define max-space-y (apply max 0 space-ys))
  (define min-space-y (apply min 0 space-ys))
  (define max-note-y (+ NOTEHEAD-RADIUS (* max-space-y STAFF-SPACE-HEIGHT)))
  (define min-note-y (+ (- NOTEHEAD-RADIUS) (* min-space-y STAFF-SPACE-HEIGHT)))
  (define start-x START-X)
  (define quarters (duration-quarters meas-dur))
  (define Ws (list start-x (* QUARTER-DUR-WIDTH quarters)))
  (define Hs (list (max (* 2 STAFF-SPACE-HEIGHT)
                        (- min-note-y))
                   (max (+ STAFF-HEIGHT (* 2 STAFF-SPACE-HEIGHT))
                        max-note-y)))
  (define (F ws hs)
    (match-define (list w1 w2) ws)
    (match-define (list h1 h2) hs)
    (define w (+ w1 w2))
    (define h (+ h1 h2))
    (define start-x w1)
    (define staff-y h1)
    (define quarter-dur-width (/ w2 quarters))
    (define mrc (meas-render-consts w h start-x staff-y quarter-dur-width))
    (place-elems
     mrc elems
     (place-barlines
      mrc
      (place-staff
       mrc
       (put-pinhole
        start-x staff-y
        (rectangle w h "outline" "transparent"))))))
  (values
   (state)
   (stretchable Ws Hs F)))

;; --------------------------------------------------------------

(define (place-staff mrc img)
  (match-define (meas-render-consts w h start-x staff-y quarter-dur-width) mrc)
  (for/fold ([img img])
            ([i (in-range 0 STAFF-NUM-LINES)])
    (define y (+ staff-y (* i STAFF-SPACE-HEIGHT)))
    (scene+line
     img
     0 y w y
     STAFF-COLOR)))

(define (place-barlines mrc img)
  (match-define (meas-render-consts w h start-x staff-y quarter-dur-width) mrc)
  (for/fold ([img img])
            ([x (in-list (list 0 w))])
    (scene+line
     img
     x staff-y
     x (+ staff-y STAFF-HEIGHT)
     STAFF-COLOR)))

(define (place-elems mrc elems img)
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
     (define y (+ staff-y (* (treble-space-y e)
                             STAFF-SPACE-HEIGHT)))
     (place-image/pinhole
      (render-notehead+stem e dur)
      x y
      (place-ledger-lines
       mrc x (treble-space-y e)
       img))]
    [else
     img]))

;; puts a pinhole in the center of the notehead
(define (render-notehead+stem n dur)
  (define head (center-pinhole (render-notehead dur)))
  (define (stem n head)
    (if (treble-stem-up? n)
        (add-line/pinhole head
                          (* 2 NOTEHEAD-RADIUS) NOTEHEAD-RADIUS
                          (* 2 NOTEHEAD-RADIUS) (- NOTEHEAD-RADIUS STEM-LENGTH)
                          NOTE-COLOR)
        (add-line/pinhole head
                          0 NOTEHEAD-RADIUS
                          0 (+ NOTEHEAD-RADIUS STEM-LENGTH)
                          NOTE-COLOR)))
  (cond
    [(duration=? dur duration-quarter) (stem n head)]
    [(duration=? dur duration-half)    (stem n head)]
    [(duration=? dur duration-whole)   head]
    [else                              head]))

(define (render-notehead dur)
  (cond
    [(duration=? dur duration-quarter)
     (circle NOTEHEAD-RADIUS "solid" NOTE-COLOR)]
    [(duration=? dur duration-half)
     (circle NOTEHEAD-RADIUS "outline" NOTE-COLOR)]
    [(duration=? dur duration-whole)
     (circle NOTEHEAD-RADIUS "outline" NOTE-COLOR)]
    [else
     (circle NOTEHEAD-RADIUS "solid" NOTE-ERROR-COLOR)]))

;; x is the x-position on the image
;; spc-y is the y-value on the staff, positive above top, negative below top
(define (place-ledger-lines mrc x spc-y img)
  (cond
    [(positive? spc-y)
     (for/fold ([img img])
               ([i (in-range STAFF-NUM-LINES (floor (add1 spc-y)) 1)])
       (place-ledger-line
        mrc x i
        img))]
    [else
     (for/fold ([img img])
               ([i (in-range -1 (ceiling (sub1 spc-y)) -1)])
       (place-ledger-line
        mrc x i
        img))]))

(define (place-ledger-line mrc x spc-y img)
  (match-define (meas-render-consts w h start-x staff-y quarter-dur-width) mrc)
  (define y (+ staff-y (* spc-y STAFF-SPACE-HEIGHT)))
  (scene+line
   img
   (- x (* 2 NOTEHEAD-RADIUS)) y
   (+ x (* 2 NOTEHEAD-RADIUS)) y
   STAFF-COLOR))

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
                    (G 3))
             (timed (time-period (position 1 beat-three) duration-half)
                    (G 4))
             (timed (time-period (position 2 beat-one) duration-whole)
                    (C 4))
             )))))
  (score->image score-C5)
  (score->image score-CDEG)
  )

;; --------------------------------------------------------------

