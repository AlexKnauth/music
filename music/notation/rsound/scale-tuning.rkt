#lang agile

(require rsound
         music/data/tuning/scale-tuning
         "frequency.rkt")
(module+ demo
  (require (submod music/data/tuning/scale-tuning example)))

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

(module+ demo
  (require racket/runtime-path)
  (define-runtime-path scale.wav "scale.wav")

  (define root-freq 220)
  (define tempo 100.0)

  (define just-major-scale-signal
    (scale-tuning->signal
     (scale-tuning root-freq just-major)
     tempo))

  (define just-major-ish-scale-signal
    (scale-tuning->signal
     (scale-tuning root-freq just-major-ish)
     tempo))

  (define 7TET-scale-signal
    (scale-tuning->signal
     (scale-tuning root-freq 7TET)
     tempo))

  ;(signal-play (with-volume 0.1 just-major-scale-signal))
  (signal-play (with-volume
                0.1
                (signal-sum
                 (let ([k (scale-tuning root-freq just-major-ish)])
                 (for/list ([f (in-list
                                (list (scale-tuning-frequency/diatonic k 0 4)
                                      (scale-tuning-frequency/diatonic k 0 6)
                                      ;(scale-tuning-frequency/diatonic k 1 1)
                                      (scale-tuning-frequency/diatonic k 1 3)))]
                            [i (in-naturals 1)])
                   (with-onset/beat tempo i 1.0 tone-fade (tone f)))))))
  #;(rs-write (signal->rsound 300000
                            (with-volume 0.3 just-major-ish-scale-signal))
            scale.wav)
  ;(signal-play (with-volume 0.1 7TET-scale-signal))

  (with-handlers ([exn:break? (λ (e) (stop) (raise e))])
    (sleep 12))
  (stop)

  )

;; ------------------------------------------------------------------------

