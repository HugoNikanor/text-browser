(use-modules (ice-9 rdelim))

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

;; Returns an association list where the car is a list of the selectors,
;; and the cdr is an assoc list where each elements car is the "selector"(?)
;; and the cdr are the attributes(?)
;;
;; Note that the returned data can contain quite a bit of unwanted whitespace,
;; so the data may need to be filtered even after
(define (parse-css file-path)
  (define char-set:formatting
    (char-set #\newline #\tab))

  (define (handle-selector-input selector)
    (string-split
      (string-delete char-set:formatting
                     (car selector))
      #\,))

  (define (handle-body-input body)
    (map (lambda (str)
           (string-split str #\:))
         ;; Delete empty strings, handles space-padding and trailing whitespace
         (filter-empty-str
           (string-split
             (string-delete char-set:formatting
                            (car body))
             #\;))))

  (let ((file (open-input-file file-path)))
    (let inner ((done '()))
      (let ((selector (read-delimited "{" file 'split))
            (body (read-delimited "}" file 'split)))
        (if (eof-object? (cdr selector))
          done
          (let ((done
                  (cons (cons (handle-selector-input selector)
                              (handle-body-input body))
                        done)))
            (if (eof-object? (cdr body))
              done
              (inner done))))))))

; (filter-empty-str(string-split attributes #\space))
