#lang racket
(require racket/trace)
(define nil '())
(define (atom? x) (and (not (pair? x))
                       (not (null? x))))

(define (my-disp out)
  (display out)
  (newline))

;;      S-list := ()     | (S-exp . S-list)
;;      S-exp  := Symbol | S-list
(define (print-list-structure x)
  (cond ((atom? x) (cond ((number? x) (number->string x))
                         ((symbol? x) (symbol->string x))
                         (else x)))
        ((null? x) "[]")
        (else (string-append "["
                             (print-list-structure (car x))
                             " . "
                             (print-list-structure (cdr x))
                             "]"))))


(my-disp (print-list-structure (cons 2 3)))
(my-disp (print-list-structure '(2)))
(my-disp (print-list-structure '(1 x 3 4)))
(newline)


;;      Take list into consideration
(define (atom->string x)
  (cond ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        (else x)))

(define (pls x) ; print-list-structure
  (define (iter list-content)
    (cond ((null? list-content) "")
          ((atom? list-content) (string-append ". " (pls list-content)))
          (else                 (string-append (pls (car list-content))
                                               (if (null? (cdr list-content)) "" " ")
                                               (iter (cdr list-content))))))
  (cond ((null? x) "[]")
        ((atom? x) (atom->string x))
        (else      (string-append "["
                                  (iter x)
                                  "]"))))

; demo
(my-disp (pls '(3 2 1)))
(my-disp (pls '((3 2 1) 2 1)))
(my-disp (pls '((3) (2) (1 2 (x)))))
(my-disp (pls (cons 2 1)))
(my-disp (pls (cons nil (cons nil (cons nil 1)))))
(my-disp (pls '(1 . 2)))
(my-disp (pls '(1 2 . 3)))
(my-disp (pls '(1 2 . ())))
'(1 2 . ())
