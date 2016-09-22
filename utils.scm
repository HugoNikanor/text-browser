(define (displayln msg)
  (display msg)
  (newline))

(define (writeln msg)
  (write msg)
  (newline))

(define (filter-empty-str strings)
  (filter (lambda (str)
            (not (equal? str "")))
          strings))
