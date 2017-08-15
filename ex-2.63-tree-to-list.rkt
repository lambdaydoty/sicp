#lang racket
(require racket/trace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;tree = (entry left right)
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoint-set x set)
  (cond ((null? set) (make-tree x 
                                '() 
                                '()))
        ((= x (car set)) set)
        ((< x (car set)) (make-tree
                           (car set) 
                           (adjoint-set x (left-branch set))
                           (right-branch set)))
        ((> x (car set)) (make-tree
                           (car set) 
                           (left-branch set)
                           (adjoint-set x (right-branch set))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree) result-list)))))
; (trace copy-to-list)
  (copy-to-list tree '()))
;(trace tree->list-1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define my-set (adjoint-set 3 (adjoint-set 1 (adjoint-set 4 (adjoint-set 1 (adjoint-set 5 (adjoint-set 9 (adjoint-set 2 (adjoint-set 6 '())))))))))

(define nil '())
(define tree-2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree-2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree-2-16-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

my-set
(tree->list-1 my-set)
(tree->list-2 my-set)
(newline)
(tree->list-1 tree-2-16-1)
(tree->list-2 tree-2-16-1)
(newline)
(tree->list-1 tree-2-16-2)
(tree->list-2 tree-2-16-2)
(newline)
(tree->list-1 tree-2-16-3)
(tree->list-2 tree-2-16-3)
(newline)
