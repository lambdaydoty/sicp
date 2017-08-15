#lang racket
(require racket/trace)



(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)                        ; nil + nil
        ((or  (null? l1) (null? l2)) #f)                        ; nil + not nil
        ((and (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))   ; symbol + symbol
        ((or  (not (pair? l1)) (not (pair? l2))) #f)            ; symbol + non-nil list
        (else (and (equal? (car l1) (car l2))                   ; non-nil lists
                   (equal? (cdr l1) (cdr l2))))))


(trace equal?)
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
