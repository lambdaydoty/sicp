#lang racket
(require racket/trace)
(define nil '())
(define (atom? x) (and (not (pair? x))
                       (not (null? x))))
;;      S-list := ()     | (S-exp . S-list)
;;      S-exp  := Symbol | S-list
(define (square x) (* x x))


(define (square-tree-direct tree)
  (cond ((null? tree) '())
        ((atom? (car tree)) (cons (square               (car tree))
                                  (square-tree-direct   (cdr tree))))
        (else               (cons (square-tree-direct   (car tree))
                                  (square-tree-direct   (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (element)
         (if (atom? element) 
           (square          element)
           (square-tree-map element))) tree))

;; demo
(define my-tree '(1 (2 (3 4) 5) (6 7)))
(display my-tree)
(display "\n")
(define (demo square-tree)
  (square-tree my-tree))

(demo square-tree-direct)
(demo square-tree-map)


