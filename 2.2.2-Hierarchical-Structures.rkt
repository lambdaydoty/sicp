#lang racket
(define nil '())

(pair? (list 1 2 3))
(pair? (list 1 2))
(pair? (list 1))
(pair? nil)
(pair? 1)

(define x (cons (list 1 2) (list 3 4)))
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(count-leaves x)
(count-leaves (list 1 2 3))

;; Mapping over trees
;; (scale-tree1)
(define xx (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (scale-tree1 tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree1 (car tree) factor)
                    (scale-tree1 (cdr tree) factor)))))
xx
(scale-tree1 xx 10)

;; (scale-tree2) ... using `map'
;; (1 (2 (3 4) 5) (6 7))
(define (scale-tree2 tree factor)
  (define (proc sub-tree)
    (if (not (pair? sub-tree))    ; Here I change the pair? into the (not pair?)
        (* sub-tree factor)
        (scale-tree2 sub-tree factor)))
  (map proc tree))
xx
(scale-tree2 xx 11)

;;*** Because the characteristic of map, we do not need to worry about the case `nil' in a list! ***
