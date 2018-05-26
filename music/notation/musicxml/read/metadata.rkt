#lang agile

(provide musicxml-elements->metadata)

(require musicxml/metadata
         music/util/txexpr
         (prefix-in data/
           (combine-in
            music/data/score/metadata)))

;; musicxml-elements->metadata : [Listof MXexpr] -> Metadata
(define (musicxml-elements->metadata elements)
  (cond
    [(empty? elements) #false]
    [else
     (data/metadata
      (musicxml->work (findf work? elements))
      (musicxml->movement-number (findf movement-number? elements))
      (musicxml->movement-title (findf movement-title? elements))
      (musicxml->creator (findf identification? elements)))]))

(define (work? v)
  (match v [(work _ _) #t] [_ #f]))

(define (movement-number? v)
  (match v [(movement-number _ _) #t] [_ #f]))

(define (movement-title? v)
  (match v [(movement-title _ _) #t] [_ #f]))

(define (identification? v)
  (match v [(identification _ _) #t] [_ #f]))

;; ----------

(define (musicxml->work mx)
  (match mx
    [#false #false]
    [(work '() '())
     (data/work #false)]
    [(work '()
       (list (work-title '() (leaf/str title))))
     (data/work title)]))

(define (musicxml->movement-number mx)
  (match mx
    [#false #false]
    [(movement-number '() '()) #false]
    [(movement-number '() (leaf/num num)) num]))

(define (musicxml->movement-title mx)
  (match mx
    [#false #false]
    [(movement-title '() '()) #false]
    [(movement-title '() (leaf/str str)) str]))

(define (musicxml->creator mx)
  (define (creator-type-composer? mx)
    (match mx
      [(creator _ _) (equal? (attr-ref mx 'type) "composer")]
      [_ #false]))
  (match (findf-txexpr mx creator-type-composer?)
    [#false #false]
    [(creator attrs (leaf/str composer))
     (data/creator composer)]))

