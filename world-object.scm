(load "dimensions.scm")

;(world-object
;  sibling-contex
;  ancestor-context
;  render-object)

(define (make-world-object)
  ;'(() () ()))
  `((siblings ())
    (ancestors ())
    (renders ())))

(define (sibling-context world-object)
  (display "sib:")
  (displayln world-object)
  (let ((data (assoc 'siblings world-object)))
    (cons (car data) (reverse (cdr data)))))

(define (ancestor-context world-object)
  (display "ans:")
  (displayln world-object)
  (let ((data (assoc 'ancestors world-object)))
    (cons (car data) (reverse (cdr data)))))

(define (render-objects world-object)
  (display "ren:")
  (displayln world-object)
  (let ((data (assoc 'renders world-object)))
    (cons (car data) (reverse (cdr data)))))

(define (last-sibling-context world-object)
  (car (list-ref world-object 0)))

(define (last-ancestor-context world-object)
  (car (list-ref world-object 1)))

(define (last-render-objects world-object)
  (car (list-ref world-object 2)))


(define (add-ancestor world-object ancestor)
  (cons* 'ancestors
         ancestor
         (cdr (ancestor-context world-object))))
       

(define (add-sibling world-object sibling)
  (cons* 'siblings
         sibling
         (cdr (sibling-context world-object))))

(define (add-render-object world-object render-object)
  (display "add:")
  (displayln world-object)
  (let ((old-renedrs (render-objects world-object)))
    (list (add-sibling world-object render-object)
          (ancestor-context world-object)
          (cons* (car old-renders)
                 render-object
                 (cdr (old-renders))))))

(define (get-world-size world-object)
  (fold dim+
        (make-dim 0 0)
        (map get-render-dimensions (render-objects world-object))))

(define (merge-worlds main-world sub-world)
  (map (lambda (object)
         (add-render-object main-world object))
       sub-world))

