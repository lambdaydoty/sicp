#lang racket


(define list-of-cons
  (list (cons 'x 1)
        (cons 'y 2)
        (cons 'z 3)))

(define list-of-lists
  (list (list 'x 1)
        (list 'y 2)
        (list 'z 3)))

(assoc 'z list-of-cons)
(assoc 'z list-of-lists)
