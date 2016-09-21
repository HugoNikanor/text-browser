(define (make-dim x y)
  (cons x y))

(define (dim+ d1 d2)
  (cons (+ (car d1) (car d2))
        (+ (cdr d1) (cdr d2))))

