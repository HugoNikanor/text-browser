;(render-object
;  (content . "Text?")
;  (dimensions 5 . 1)
;  (style (bg-color blue)
;         (color red)))

(define (make-render-object tag object dimensions style)
  ;`((content . ,content)
  ;    (dimensions . ,dimensions)
  ;    (style . ,style)))
  (list tag object dimensions style))

(define (get-render-tag render-object)
  ;(assoc-ref render-object 'content))
  (list-ref render-object 0))

(define (get-render-content render-object)
  ;(assoc-ref render-object 'content))
  (list-ref render-object 1))

(define (get-render-dimensions render-object)
  ;(assoc-ref render-object 'dimensions))
  (list-ref render-object 2))

(define (get-render-style render-object)
  ;(assoc-ref render-object 'style))
  (list-ref render-object 3))
