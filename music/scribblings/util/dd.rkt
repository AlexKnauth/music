#lang racket/base

(provide dd declare-dd)

(require syntax/parse/define
         (only-in scribble/manual racket deftech tech)
         (for-syntax racket/base
                     syntax/transformer
                     syntax/parse/class/local-value
                     "id-transformer.rkt"))

(begin-for-syntax
  (struct declared-dd [internal-name]
    #:property prop:procedure
    (λ (this stx)
      (define trans
        (set!-transformer-procedure
         (make-variable-like-transformer (declared-dd-internal-name this))))
      (trans stx)))

  (define-syntax-class (dd-id tech-defs)
    [pattern (~and (~var _ (local-value declared-dd?))
                   name)
             #:with norm (if (member #'name tech-defs free-identifier=?)
                             #'#,(deftech name)
                             #'#,name)])

  (define-syntax-class (dd tech-defs)
    #:attributes [norm]
    [pattern name
             #:declare name (dd-id tech-defs)
             #:with norm #'name.norm]
    [pattern other:id
             #:with norm #'other]
    [pattern [name d ...]
             #:declare name (dd-id tech-defs)
             #:declare d (dd tech-defs)
             #:with norm #'[name.norm d.norm ...]]))

(define-simple-macro (declare-dd name:id)
  #:with name-str (symbol->string (syntax-e #'name))
  (begin
    (define-syntax name (declared-dd #'internal))
    (define internal (tech 'name-str))))

(define-syntax-parser dd
  [(_ d)
   #:declare d (dd '())
   #'(racket d.norm)]
  [(_ #:def (~and d (~or name:id [name:id . rst])))
   #:declare d (dd (list #'name))
   #'(racket d.norm)])

