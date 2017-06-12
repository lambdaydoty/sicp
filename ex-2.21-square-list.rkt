#lang racket
(require racket/trace)



(define (square-list1 items)
  (if (null? items)
    '()
    (cons (* (car items) (car items)) 
          (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(define (demo square-list)
  (begin
    (display (square-list '(1 2 3 4)))   (display "\n")))

(demo square-list1)
(demo square-list2)
