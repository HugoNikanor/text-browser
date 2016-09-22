(use-modules (ice-9 rdelim))

(load "utils.scm")

;; Returns an association list where the car is a list of the selectors,
;; and the cdr is an assoc list where each elements car is the "selector"(?)
;; and the cdr are the attributes(?)
;;
;; Note that the returned data can contain quite a bit of unwanted whitespace,
;; so the data may need to be filtered even after
(define (parse-css file-path)

  ;; Returns a list with each selector (as defined by comma separation)
  ;; as a string in a list, with non-semantic whitespace stripped
  (define (handle-selector-input selector)
    (map (lambda (str)
           (string-trim-both str char-set:whitespace))
         (string-split (car selector)
                       #\,)))

  ;; Returns the body as a number of key-value pairs, where the first element
  ;; is the key, and the rest are values.
  ;; All non-sematic whitespace is stripped
  (define (handle-body-input body)
    ;; Removes the whitespace caused by the space between the last
    ;; semicolon and the end curly brace
    (filter (lambda (key-value)
              (not (and (= (length key-value) 1)
                        (equal? (car key-value) ""))))
            (map (lambda (str)
                   (let ((key-value (string-split str #\:)))
                     ;; possibly change this to cons
                     (map (lambda (inner-str)
                            (string-trim-both inner-str char-set:whitespace))
                          key-value)))
                 ;; Delete empty strings, this is needed since there
                 ;; may be non semantic whitespace between the last
                 ;; element's semicolon and the end curly brace
                 (string-split (car body) #\;))))

  (let ((file (open-input-file file-path)))
    (let inner ((done '()))
      ;; This builds upon the fact that file readers are destructive
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

;; select should be a list of strings, on some form
;; css should be on the form returned from parse-css
;; TODO this is currently basicly horrible, rewrite
(define (get-appropriate-css selector css)
  (call/cc (lambda (return)
             (for-each (lambda (key-value)
                         (when (equal? (car key-value)
                                       selector)
                           (return (cdr key-value))))
                       css)
             (return '()))))
