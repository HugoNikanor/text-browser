(use-modules (ncurses curses))

(define stdscr (initscr))

(define (move-c coordinate)
  (move (cdr top-left)
        (car top-left)))

;; TODO
(define (draw-box size)
  (hline stdscr (normal #\-) (cdr size))
  (vline stdscr (normal #\|) (car size))
  (move 

;; Draws the "container" part of an object,
;; unless it's an atom, then draw that
(define (draw-object tag dim)
  (case tag
    ((div)
     ;; Draws a border
     )
    ((p)
     ;; Adds the string
     )))

(define (display-render-tree render-tree top-left)
  (clear-screen!)
  (fold (lambda (render-object top-left)
          (move-c top-left)
          (draw-object (get-render-tag render-object)
                       (get-render-dimmensions render-object))
          (display-render-tree (get-render-content render-object)
                               top-left))
        top-left
        render-tree))
