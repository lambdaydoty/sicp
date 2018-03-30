#lang racket

(define (make-tree datum children)  (cons datum children))
(define (datum node)                (car node))
(define (children node)             (cdr node))
(define (leaf? node)                (null? (children node)))

;; map over trees
(define (treemap proc tree)
  (make-tree (proc (datum tree))
             (forest-map proc (children tree))))
(define (forest-map proc forest)
  (map (lambda (t) (treemap proc t)) forest))


;; define a tree example
(define my-tree0 (make-tree 'C '()))
(define my-tree1 (make-tree 'E '()))
(define my-tree2 (make-tree 'D (list my-tree0 my-tree1)))
(define my-tree3 (make-tree 'B (list (make-tree 'A '())
                                     my-tree2)))

(define my-tree4 (make-tree 'H '()))
(define my-tree5 (make-tree 'I (list my-tree4)))
(define my-tree6 (make-tree 'G (list my-tree5)))

(define my-tree  (make-tree 'F (list my-tree3 my-tree6)))

my-tree
(treemap symbol->string my-tree)

;; BFS & DFS
(define (depth-first-search proc! tree)
  (define (recur t)
    (proc! (datum t))
    (for-each recur (children t)))
  (recur tree))
(define (breadth-first-earch proc! tree) ; need a queue structure
  (define (iter queue)
    (if (null? queue)
      'done
      (let ([task (car queue)])
        (proc! (datum task))
        (iter (append (cdr queue) (children task))))))
  (iter (list tree)))

(newline)
(depth-first-search (lambda (x)
                      (display x)
                      (newline)) my-tree)
(newline)
(breadth-first-earch (lambda (x)
                      (display x)
                      (newline)) my-tree)
         




