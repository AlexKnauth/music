#lang agile

(provide with-volume
         with-onset/beat
         tone tones tone-fade
         bell bells bell-fade
         )

(require racket/math
         rsound)

;; frames per second
(define sample-rate 44100)

;; ------------------------------------------------------------------------

(define (fader/on-off t0 duration fade-frames)
  (let ([p (expt 0.001 (/ 1 fade-frames))]
        [end (+ t0 duration)])
    (network ()
      [t <= frame-ctr]
      [out = (cond [(< end t) 0.0]
                   [(< t0 t) (* p (prev out 1.0))]
                   [(= t0 t) 1.0]
                   [else     0.0])])))

;; ------------------------------------------------------------------------

(define bell-overtones
  (network (freq)
    [t1 <= sine-wave (* 1 freq)]
    [t2 <= sine-wave (* 2 freq)]
    [t3 <= sine-wave (* 3 freq)]
    [t4 <= sine-wave (* 4.2 freq)]
    [t5 <= sine-wave (* 5.4 freq)]
    [t6 <= sine-wave (* 6.8 freq)]
    [out = (+ (* 1 t1)
              (* 0.6 t2)
              (* 0.4 t3)
              (* 0.25 t4)
              (* 0.2 t5)
              (* 0.15 t6)
              )]))

;; ------------------------------------------------------------------------

(define bell-fade 200000)
(define tone-fade 600000)

(define (with-volume volume signal)
  (clip&volume volume signal))

(define (with-onset t0 duration fade-frames signal)
  (signal-* (fader/on-off t0 duration fade-frames) signal))

(define (tone freq)
  (network ()
    [out <= sine-wave freq]))

(define (bell freq)
  (network ()
    [out <= bell-overtones freq]))

(define (tones . freqs)
  (signal-+s (map tone freqs)))
(define (bells . freqs)
  (signal-+s (map bell freqs)))

;; ------------------------------------------------------------------------

(define (beat->time tempo b)
  (exact-round (* b (/ tempo) 60 sample-rate)))

(define (with-onset/beat tempo b0 duration fade-frames signal)
  (with-onset (beat->time tempo b0)
              (beat->time tempo duration)
              fade-frames
              signal))

;; ------------------------------------------------------------------------

