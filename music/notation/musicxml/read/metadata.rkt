#lang agile

(provide musicxml-elements->metadata)

(require music/util/txexpr
         (prefix-in data/
           (combine-in
            music/data/score/metadata)))

;; musicxml-elements->metadata : [Listof MXexpr] -> Metadata
(define (musicxml-elements->metadata elements)
  (cond
    [(empty? elements) #false]
    [else
     (data/metadata
      (musicxml->work (find/tag elements 'work))
      (musicxml->movement-number (find/tag elements 'movement-number))
      (musicxml->movement-title (find/tag elements 'movement-title))
      (musicxml->creator (find/tag elements 'identification)))]))

;; find/tag : [Listof MXexpr] TXexprTag -> [Maybe MXexpr]
(define (find/tag mxs tag)
  (for/first ([mx (in-list mxs)]
              #:when (equal? (get-tag mx) tag))
    mx))

(define (musicxml->work mx)
  (match mx
    [#false #false]
    [(txexpr 'work '() '())
     (data/work #false)]
    [(txexpr 'work '()
       (list (txexpr 'work-title '() (leaf/str title))))
     (data/work title)]))

(define (musicxml->movement-number mx)
  (match mx
    [#false #false]
    [(txexpr 'movement-number '() '()) #false]
    [(txexpr 'movement-number '() (leaf/num num)) num]))

(define (musicxml->movement-title mx)
  (match mx
    [#false #false]
    [(txexpr 'movement-title '() '()) #false]
    [(txexpr 'movement-title '() (leaf/str str)) str]))

(define (musicxml->creator mx)
  (define (creator-type-composer? mx)
    (and (txexpr? mx)
         (equal? (get-tag mx) 'creator)
         (equal? (attr-ref mx 'type) "composer")))
  (match (findf-txexpr mx creator-type-composer?)
    [#false #false]
    [(txexpr 'creator attrs (leaf/str composer))
     (data/creator composer)]))

