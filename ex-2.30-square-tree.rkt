#lang racket
(require racket/trace)


(define (square-tree1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree) 
           (square-tree2 sub-tree)
           (* sub-tree sub-tree)))
       tree))

(define (square-tree3 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (map (lambda (t) (square-tree3 t)) tree))))

(define (demo square-tree)
  (define x '(1 (2 (3 4) 5) (6 7)))
  (display (square-tree x))
  (display "\n"))

;(trace fringe1)
(demo square-tree1)
(demo square-tree2)
(demo square-tree3)


