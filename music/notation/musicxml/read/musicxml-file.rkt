#lang at-exp agile

(require xml)

;; ------------------------------------------------------------------------

(provide read-musicxml-file)

;; read-musicxml-file : PathString -> MXexpr
(define (read-musicxml-file file-path)
  (call-with-input-file* file-path read-musicxml))

;; read-musicxml : InputPort -> MXexpr
(define (read-musicxml in)
  (match (read-xml in)
    [(document prolog element '())
     (check-musicxml-prolog prolog)
     (xml->xexpr element)]))

;; check-musicxml-prolog : 
(define (check-musicxml-prolog p)
  (match p
    [(prolog misc1 dtd '())
     (void)]))

;; ------------------------------------------------------------------------

