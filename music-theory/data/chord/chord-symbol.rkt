#lang agile

(require "chord.rkt"
         "../../util/defs.rkt")
(module+ example
  (provide (all-defined-out))
  (require (for-syntax racket/syntax)))

;; ------------------------------------------------------------------------

(provide chord-symbol chord-symbol-root
         chord-symbol-kind
         )

;; A ChordSymbol is a (chord-symbol Note ChordSymbolKind)
(struct chord-symbol [root symkind] #:transparent)

;; A ChordSymbolKind is a (chord-symbol-kind String ChordKind)
(struct chord-symbol-kind [name ivls] #:transparent)

;; The `name` field must match up with the MusicXML definition for the
;; string inside the kind element here:
;; <harmony>
;;   <root> ... </root>
;;   <kind> `name` </kind>
;;   <frame> ... </frame>
;; </harmony>
;; For more information see `direction.mod` in the MusicXML type definition

(module+ example
  (define-simple-macro
    (defs/chord-symbol-kind prefix [name:id ivls:expr] ...)
    #:with [[x:id s:str] ...]
    (for/list ([n (in-list (syntax->list #'[name ...]))])
      (list (format-id n "~a~a" #'prefix n #:source n #:props n)
            (symbol->string (syntax-e n))))
    (defs [x (chord-symbol-kind 's ivls)] ...))

  ;; Don't add more without consulting the MusicXML definition,
  ;; specifically the definition for chord kinds in `direction.mod`
  (defs/chord-symbol-kind chord-symbol-kind:
    ;; Triads:
    [major major-triad]
    [minor minor-triad]
    [augmented augmented-triad]
    [diminished diminished-triad]

    ;; Sevenths:
    [dominant dominant-7]
    [major-seventh major-7]
    [minor-seventh minor-7]
    [diminished-seventh diminished-7]
    [half-diminished half-diminished-7]
    ))

;; ------------------------------------------------------------------------

;; Conversions

(provide chord-symbol->chord
         chord-symbol-kind->chord-kind)

;; chord-symbol->chord : ChordSymbol -> Chord
(define (chord-symbol->chord cs)
  (match cs
    [(chord-symbol root kind)
     (chord root (chord-symbol-kind->chord-kind kind))]))

;; chord-symbol-kind->chord-kind : ChordSymbolKind -> ChordKind
(define (chord-symbol-kind->chord-kind csk)
  (chord-symbol-kind-ivls csk))

;; ------------------------------------------------------------------------

