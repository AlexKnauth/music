#lang agile

(require rsound
         "../../data/tuning/scale-tuning.rkt"
         "frequency.rkt")
(module+ main
  (require (submod "../../data/tuning/scale-tuning.rkt" example)))

(define (signal-sum . signals)
  (signal-+s (flatten signals)))

;; ------------------------------------------------------------------------

;; scale-tuning->signal : ScaleTuning PosReal -> Signal
(define (scale-tuning->signal st tempo)
  (define n (scale-tuning-number st))
  (signal-sum
   (for/list ([i (in-range n)])
     (let ([f (scale-tuning-frequency/diatonic st 0 i)])
       (with-onset/beat tempo i 1.0 tone-fade (tone f))))
   (let ([f (scale-tuning-frequency/diatonic st 1 0)])
     (with-onset/beat tempo n 4.0 tone-fade (tone f)))))

;; ------------------------------------------------------------------------

(module+ main

  (define root-freq 220)
  (define tempo 100.0)

  (define just-major-scale-signal
    (scale-tuning->signal
     (scale-tuning root-freq just-major)
     tempo))

  (define 7TET-scale-signal
    (scale-tuning->signal
     (scale-tuning root-freq 7TET)
     tempo))

  (signal-play (with-volume 0.1 just-major-scale-signal))
  ;(signal-play (with-volume 0.1 7TET-scale-signal))

  (with-handlers ([exn:break? (λ (e) (stop) (raise e))])
    (sleep 12))
  (stop)

  )

;; ------------------------------------------------------------------------

