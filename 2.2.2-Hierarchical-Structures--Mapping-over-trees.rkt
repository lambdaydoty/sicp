#lang racket
(define nil '())
(define (atom? x) (and (not (pair? x))
                       (not (null? x))))

(define (scale-tree-sicp tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree-sicp (car tree) factor)
                    (scale-tree-sicp (cdr tree) factor)))))

;; A better way to look at a tree, actually, a `list' of S expressions:
;;      S-list := ()     | (S-exp . S-list)
;;      S-exp  := Symbol | S-list
(define (scale-tree-tls tree factor)
  (cond ((null? tree) nil)
        ((atom? (car tree)) (cons (*              (car tree) factor)
                                  (scale-tree-tls (cdr tree) factor)))
        (else (cons (scale-tree-tls (car tree) factor)
                    (scale-tree-tls (cdr tree) factor)))))

;; (scale-tree-map) ... using `map'
(define (scale-tree-map tree factor)
  (map (lambda (element)
         (if (atom? element)    ; here atom? is better than pair?
           (* element factor)   ; cuz. '() is still regarded as a tree
           (scale-tree-map element factor))) tree))


;; demo
(define my-tree '(1 (2 (3 4) 5) (6 7)))
(display my-tree)
(display "\n")
(define (demo scale-tree)
  (scale-tree my-tree 10))
(demo scale-tree-sicp)
(demo scale-tree-map)
(demo scale-tree-tls)



