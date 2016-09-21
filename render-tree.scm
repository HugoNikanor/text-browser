(define (make-empty-render-tree)
  '())

(define (add-render-object tree object)
  (cons object tree))

(define (get-render-objects tree)
  (reverse tree))
