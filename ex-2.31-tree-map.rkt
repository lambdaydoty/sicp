#lang racket
(require racket/trace)


(define (tree-map1 proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map1 proc (car tree))
                    (tree-map1 proc (cdr tree))))))

(define (tree-map2 proc tree)
  (map (lambda (t)
         (if (pair? t) 
           (tree-map2 proc t)
           (proc t)))
       tree))

(define (tree-map3 proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (map (lambda (t) (tree-map3 proc t)) tree))))

(define (demo tree-map)
  (define x '(1 (2 (3 4) 5) (6 7)))
  (define (square z) (* z z))
  (display (tree-map square x))
  (display "\n"))

(demo tree-map1)
(demo tree-map2)
(demo tree-map3)


