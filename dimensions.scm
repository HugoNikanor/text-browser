(define (make-dim x y)
  (cons x y))

(define (dim-op fun d1 d2)
  (cons (fun (car d1) (car d2))
        (fun (cdr d1) (cdr d2))))

(define (dim+ d1 d2)
  (dim-op + d1 d2))

(define (dim- d1 d2)
  (dim-op - d1 d2))
