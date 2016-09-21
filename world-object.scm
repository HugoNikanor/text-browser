
;(world-object
;  sibling-contex
;  ancestor-context
;  render-object)

(define (make-world-object)
  '(() () ()))

(define (sibling-context world-object)
  (reverse (list-ref world-object 0)))

(define (ancestor-context world-object)
  (reverse (list-ref world-object 1)))

(define (render-objects world-object)
  (reverse (list-ref world-object 2)))


(define (last-sibling-context world-object)
  (car (list-ref world-object 0)))

(define (last-ancestor-context world-object)
  (car (list-ref world-object 1)))

(define (last-render-objects world-object)
  (car (list-ref world-object 2)))


(define (add-ancestor world-object ancestor)
  (list (sibling-context world-object)
        (cons ancestor (ancestor-context world-object))
        (render-objects world-object)))

(define (add-sibling world-object sibling)
  (list (cons sibling (sibling-context world-object))
        (ancestor-context world-object)
        (render-objects world-object)))

(define (add-render-object world-object render-object)
  (list (sibling-context world-object)
        (ancestor-context world-object)
        (cons render-object (render-objects world-object))))
