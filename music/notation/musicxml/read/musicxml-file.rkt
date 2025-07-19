#lang at-exp agile

(require (submod txexpr safe)
         xml
         (only-in scribble/decode whitespace?))

;; ------------------------------------------------------------------------

(provide read-musicxml-file)

;; destroy-all-whitespace : XExpr -> XExpr
(module+ test
  (check-txexprs-equal? (destroy-all-whitespace
                         '(note
                           ()
                           " "
                           (pitch () " " (step () "B") " " (octave () "4") " ")
                           " "
                           (duration () "1")
                           " "
                           (voice () "1")
                           " "
                           (type () "eighth")
                           " "
                           (notations ())
                           " "))
                        '(note
                          (pitch (step "B") (octave "4"))
                          (duration "1")
                          (voice "1")
                          (type "eighth")
                          (notations))))

(define (destroy-all-whitespace xexpr)
  (define-values [x _o] (splitf-txexpr xexpr whitespace?))
  x)

;; read-musicxml-file : PathString -> MXexpr
(define (read-musicxml-file file-path)
  (call-with-input-file* file-path read-musicxml))

;; read-musicxml : InputPort -> MXexpr
(define (read-musicxml in)
  (parameterize ([collapse-whitespace #true])
    (match (read-xml in)
      [(document prolog element '())
       (check-musicxml-prolog prolog)
       (destroy-all-whitespace (xml->xexpr element))])))

;; check-musicxml-prolog : 
(define (check-musicxml-prolog p)
  (match p
    [(prolog misc1 dtd '())
     (void)]))

;; ------------------------------------------------------------------------

