(use-modules (sxml simple)
             (srfi srfi-1)
             (texinfo string-utils))

(load "parse-css.scm")
;(load "world-object.scm")
(load "render-object.scm")
(load "render-tree.scm")
(load "dimmensions.scm")
(load "utils.scm")

(define (display-webpage-fragment html-file-path)
  (map handle-html (cdr (file-path->sxml html-file-path))))

(define (file-path->sxml file-path)
  (xml->sxml
    (read-string
      (open-input-file file-path))
    #:trim-whitespace? #t))

(define (get-attributes tag)
  (cond
    ((null? tag)
     '())
    ((null? (cdr tag))
     '())
    ((eqv? (caadr tag)
           '@)
     (cdadr tag))
    (else
      '())))

;; TODO change map to fold
;; TODO write fold logic

;(fold (lambda (page-state next-page-element)

(define (html-tag? node)
  (and (list? node)
       (not (null? node))
       (symbol? (car node))))

(define (create-render-tree data)
  (fold (lambda (node render-tree)
          (if (not (html-tag? node))
            ;; Atom / Leaf
            (if (string? node)
              (add-render-object
                render-tree
                (make-render-object
                  'string
                  node
                  (make-dim (string-length node) 1)
                  '()))
              ;; else list
              (let ((content (car node)))
                (cond
                  ((string? content)
                   (add-render-object
                     render-tree
                     (make-render-object
                       'string-list
                       content
                       (make-dim (string-length content)
                             ;; TODO replace 80 with container width
                             (remainder string-length 80))
                       '())))
                  (else
                    (add-render-object
                      render-tree
                      (make-render-object 'atom "" (make-dim 0 0) '()))))))

            ;; Node / Branch
            ;; Note that body might quite often be empty
            (let ((tag (car node))
                  (body (cdr node)))
              (let ((inner-tree (get-render-objects
                                  (create-render-tree body))))
                (display "it:")
                (displayln inner-tree)
                (let ((inner-dim (fold dim+
                                       (make-dim 0 0)
                                       (map get-render-dimensions inner-tree))))
                  (add-render-object
                    render-tree
                    ;; THIS IS WHERE MARGINS, CSS/DISPLAY, PADDINGS,
                    ;; AND BORDERS SHOULD BE CALCULATED
                    ;;
                    ;; TODO this is for div, do different things
                    ;; depending on the object type
                    (make-render-object tag
                                        inner-tree
                                        (case tag
                                          ((div)
                                           (dim+ inner-dim
                                                 (make-dim 2 2)))
                                          (else inner-dim))
                                        '())))))))
        (make-empty-render-tree)
        data))


(define (handle-html data style)
  (if (null? data)
    #f
    (case (car data)
      ((html)
       (map handle-html (cdr data)))
      ((head)
       (map handle-html (cdr data)))
      ;;((link)
      ;; (let ((atr (get-attributes data)))
         ;; TODO replace with case
         ;;(cond
         ;;  ((equal? (assoc-ref atr 'type)
         ;;          "styleheet")
         ;;   (append style
         ;;           (parse-css (assoc-ref atr 'href)))
         ;;  )
      ((title)
       (display (center-string (cadr data)
                               80
                               #\=))
       (newline))
      ((body)
       (map handle-html (cdr data)))
      ((div)
       ;; Create display box
       ;; display content in that box
       'div)
      ((p)
       ;; Display string
       'p)
      (else 'else))))
