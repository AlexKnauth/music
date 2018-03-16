#lang racket/base

(provide stretchable
         stretchable-min-ws
         stretchable-min-hs
         stretchable-render
         stretchable-render/min-size
         stretchable-beside
         stretchable-above)

(require racket/list
         racket/match)

(define (avg x y)
  (* 1/2 (+ x y)))

;; --------------------------------------------------------------

;; A [Stretchable Img] is a
;;  (stretchable [Listof PosReal]
;;               [Listof PosReal]
;;               [[Listof PosReal] [Listof PosReal] -> Img])
(struct stretchable [min-ws min-hs render-fn])
;; - the length of min-ws represents the number of degrees of
;;   freedom in the width, and each element represents the min
;;   value for that degree of freedom
;; - the length of min-hs represents the number of degrees of
;;   freedom in the height, and each element represents the min
;;   value for that degree of freedom
;; - the render-fn field takes two lists `ws` and `hs`
;;   such that ws[i] >= min-ws[i] ... and hs[j] >= min-hs[j] ...

;; stretchable-render :
;; ∀ Img .
;;   [Stretchable Img] [Listof PosReal] [Listof PosReal] -> Img
(define (stretchable-render s-img ws hs)
  (match s-img
    [(stretchable Ws Hs F)
     (unless (= (length ws) (length Ws))
       (error 'stretchable-render "expected ~v w values, given ~v"
              (length Ws) (length ws)))
     (unless (= (length hs) (length Hs))
       (error 'stretchable-render "expected ~v h values, given ~v"
              (length Hs) (length hs)))
     (for ([w (in-list ws)]
           [W (in-list Ws)])
       (unless (>= w W)
         (error 'stretchable-render "expected a w value >= ~v, given ~v" W w)))
     (for ([h (in-list hs)]
           [H (in-list Hs)])
       (unless (>= h H)
         (error 'stretchable-render "expected an h value >= ~v, given ~v" H h)))
     (F ws hs)]))

;; stretchable-render/min-size :
;; ∀ Img .
;;   [Stretchable Img] -> Img
(define (stretchable-render/min-size s-img)
  (stretchable-render s-img
                      (stretchable-min-ws s-img)
                      (stretchable-min-hs s-img)))

;; stretchable-beside :
;; ∀ Img .
;;   [Img Img -> Img]
;;   ->
;;   [[Stretchable Img] [Stretchable Img] -> [Stretchable Img]]
;; the images must have the same number of H degrees of freedom,
;; but not necessarily the same number of W degrees of freedom
(define ((stretchable-beside beside) a b)
  (match* [a b]
    [[(stretchable aW aH aF) (stretchable bW bH bF)]
     (define aWN (length aW))
     (define aHN (length aH))
     (define bHN (length bH))
     (unless (= aHN bHN)
       (error 'stretchable-beside "expected the same h degrees of freedom"))
     ;; aH and bH are the same length,
     ;; H is the same length as aH and bH
     (define W (append aW bW))
     (define H (map max aH bH))
     (define (F w h)
       ;; ASSUME w[i] >= W[i] and h[i] >= H[i]
       (define-values [aw bw]
         (split-at w aWN))
       (define a-img (aF aw h))
       (define b-img (bF bw h))
       (beside a-img b-img))
     (stretchable W H F)]))

;; stretchable-above :
;; ∀ Img .
;;   [Img Img -> Img]
;;   ->
;;   [[Stretchable Img] [Stretchable Img] -> [Stretchable Img]]
;; the images must have the same number of W degrees of freedom,
;; but not necessarily the same number of H degrees of freedom
(define ((stretchable-above above) a b)
  (match* [a b]
    [[(stretchable aW aH aF) (stretchable bW bH bF)]
     (define aWN (length aW))
     (define bWN (length bW))
     (define aHN (length aH))
     (unless (= aWN bWN)
       (error 'stretchable-above "expected the same w degrees of freedom"))
     ;; aW and bW are the same length,
     ;; W is the same length as aW and bW
     (define W (map max aW bW))
     (define H (append aH bH))
     (define (F w h)
       ;; ASSUME w[i] >= W[i] and h[i] >= H[i]
       (define-values [ah bh]
         (split-at h aHN))
       (define a-img (aF w ah))
       (define b-img (bF w bh))
       (above a-img b-img))
     (stretchable W H F)]))

;; --------------------------------------------------------------

