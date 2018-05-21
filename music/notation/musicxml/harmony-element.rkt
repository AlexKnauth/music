#lang agile

(require racket/bool
         musicxml/harmony
         (prefix-in data/
           (combine-in
            music/data/note/note
            music/data/score/score
            music/data/chord/chord-symbol)))

;; ------------------------------------------------------------------------

(provide harmony-element->musicxml)

;; harmony-element->musicxml : HarmonyElement -> MXexpr
(define (harmony-element->musicxml he)
  (match he
    [(data/harmony-element
      (data/chord-symbol r
                         k)
      layout)
     (cond
       [(false? layout)
        (harmony '([print-frame "no"])
          (list
           (harmony-root->musicxml r)
           (harmony-kind->musicxml k)))]
       [else
        (harmony '([print-frame "no"])
          (list
           (harmony-root->musicxml r)
           (harmony-kind->musicxml k)
           (harmony-frame->musicxml layout)))])]))

;; harmony-root->musicxml : Note -> MXexpr
(define (harmony-root->musicxml r)
  (define n (data/note-name-string r))
  (define a (data/note-alteration r))
  (cond [(zero? a)
         (root '()
           (list
            (root-step '() (list n))))]
        [else
         (root '()
           (list
            (root-step '() (list n))
            (root-alter '() (list (number->string a)))))]))

;; harmony-kind->musicxml : ChordSymbolKind -> MXexpr
(define (harmony-kind->musicxml k)
  (match k
    [(data/chord-symbol-kind kind-str _)
     (kind '() (list kind-str))]))

;; harmony-frame->musicxml : ChordLayout -> MXexpr
(define (harmony-frame->musicxml chord-layout)
  (define n (length chord-layout))
  (define k
    (add1 (apply max 0
            (map data/ivl-midi∆ (filter values chord-layout)))))
  (frame '()
    (list*
     (frame-strings '() (list (number->string n)))
     (frame-frets '() (list (number->string k)))
     (for/list ([s (in-list chord-layout)]
                [i (in-range n 0 -1)]
                #:when s)
       (define f (data/ivl-midi∆ s))
       (frame-note '()
         (list
          (string '() (list (number->string i)))
          (fret '() (list (number->string f)))))))))

;; ------------------------------------------------------------------------

