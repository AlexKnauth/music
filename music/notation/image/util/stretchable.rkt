#lang racket/base

(provide stretchable
         stretchable-render
         stretchable-render/min-size
         stretchable-beside
         stretchable-above)

(require racket/match)

(define (avg x y)
  (* 1/2 (+ x y)))

;; --------------------------------------------------------------

;; A [Stretchable Img] is a
;;  (stretchable PosReal PosReal [PosReal PosReal -> Img])
(struct stretchable [min-width min-height render-fn])
;; the render-fn field takes two arguments, w and h
;; such that w >= min-width and h >= min-height

;; stretchable-render :
;; ∀ Img .
;;   [Stretchable Img] PosReal PosReal -> Img
(define (stretchable-render s-img w h)
  (match s-img
    [(stretchable W H F)
     (unless (>= w W)
       (error 'stretchable-render "expected a width >= ~v, given ~v" W w))
     (unless (>= h H)
       (error 'stretchable-render "expected a height >= ~v, given ~v" H h))
     (F w h)]))

;; stretchable-render/min-size :
;; ∀ Img .
;;   [Stretchable Img] -> Img
(define (stretchable-render/min-size s-img)
  (stretchable-render s-img
                      (stretchable-min-width s-img)
                      (stretchable-min-height s-img)))

;; stretchable-beside :
;; ∀ Img .
;;   [Img Img -> Img]
;;   ->
;;   [[Stretchable Img] [Stretchable Img] -> [Stretchable Img]]
(define ((stretchable-beside beside) a b)
  (match* [a b]
    [[(stretchable aW aH aF) (stretchable bW bH bF)]
     (define W (+ aW bW))
     (define H (max aH bH))
     (define (F w h)
       ;; ASSUME w >= W and h >= H
       (define x-min aW)
       (define x-max (- w bW))
       (define x (avg x-min x-max))
       (define a-img (aF x h))
       (define b-img (bF (- w x) h))
       (beside a-img b-img))
     (stretchable W H F)]))

;; stretchable-above :
;; ∀ Img .
;;   [Img Img -> Img]
;;   ->
;;   [[Stretchable Img] [Stretchable Img] -> [Stretchable Img]]
(define ((stretchable-above above) a b)
  (match* [a b]
    [[(stretchable aW aH aF) (stretchable bW bH bF)]
     (define W (max aW bW))
     (define H (+ aH bH))
     (define (F w h)
       ;; ASSUME w >= W and h >= H
       (define y-min aH)
       (define y-max (- h bH))
       (define y (avg y-min y-max))
       (define a-img (aF w y))
       (define b-img (bF w (- h y)))
       (above a-img b-img))
     (stretchable W H F)]))

;; --------------------------------------------------------------

