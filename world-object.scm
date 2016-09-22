
;(world-object
;  sibling-contex
;  ancestor-context
;  render-objects)

(define (make-empty-world-object)
  '(() () () (0 . 0)))

(define (make-world-object
          sibling-context
          ancestor-context
          render-objects
          position)
  (list sibling-context
        ancestor-context
        render-objects
        position))

(define (sibling-context world-object)
  (reverse (list-ref world-object 0)))

(define (ancestor-context world-object)
  (reverse (list-ref world-object 1)))

(define (render-objects world-object)
  (reverse (list-ref world-object 2)))

(define (world-position world-object)
  (list-ref world-object 3))

(define (set-world-position world-object new-position)
  (list (sibling-context world-object)
        (ancestor-context world-object)
        (render-objects world-object)
        new-position))

(define (move-world-position world-object change)
  (set-world-position (dim+ (world-position world-object)
                            change)))


(define (last-sibling-context world-object)
  (car (list-ref world-object 0)))

(define (last-ancestor-context world-object)
  (car (list-ref world-object 1)))

(define (last-render-objects world-object)
  (car (list-ref world-object 2)))


(define (add-ancestor world-object ancestor)
  (list (sibling-context world-object)
        (cons ancestor (ancestor-context world-object))
        (render-objects world-object)
        (world-position world-object)))

(define (add-sibling world-object sibling)
  (list (cons sibling (sibling-context world-object))
        (ancestor-context world-object)
        (render-objects world-object)
        (world-position world-object)))

(define (add-render-object world-object render-object)
  (list (sibling-context world-object)
        (ancestor-context world-object)
        (cons render-object (render-objects world-object))
        (move-world-position world-object
                             (get-render-dimensions render-object))))
