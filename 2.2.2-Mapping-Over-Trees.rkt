#lang racket
(require racket/trace)


(define (scale-tree1 tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree1 (car tree) factor)
                    (scale-tree1 (cdr tree) factor)))))

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree) 
           (scale-tree2 sub-tree factor)
           (* sub-tree factor)))
       tree))

(define (scale-tree3 tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (map (lambda (t) (scale-tree3 t factor)) tree))))

(define (demo scale-tree)
  (define x '(1 (2 (3 4) 5) (6 7)))
  (display (scale-tree x 10))
  (display "\n"))

;(trace fringe1)
(demo scale-tree1)
(demo scale-tree2)
(demo scale-tree3)


