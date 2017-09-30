#lang racket
(require racket/trace)
(define nil '())
(define (atom? x) (and (not (pair? x))
                       (not (null? x))))
;;      S-list := ()     | (S-exp . S-list)
;;      S-exp  := Symbol | S-list
(define (square x) (* x x))


(define (tree-map1 proc tree)
  (cond ((null? tree) '())
        ((atom? (car tree)) (cons (proc           (car tree))
                                  (tree-map1 proc (cdr tree))))
        (else               (cons (tree-map1 proc (car tree))
                                  (tree-map1 proc (cdr tree))))))

(define (tree-map2 proc tree)
  (map (lambda (element)
         (if (atom? element) 
           (proc           element)
           (tree-map2 proc element))) tree))

;; demo
(define my-tree '(1 (2 (3 4) 5) (6 7)))
(display my-tree)
(display "\n")
(define (demo tree-map)
  (tree-map square my-tree))

(demo tree-map1)
(demo tree-map2)

