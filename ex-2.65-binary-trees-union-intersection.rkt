#lang racket
(require racket/trace)


;; 2.3.3-Sets-as-binary-trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; ex-2.63
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

;; ex-2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)       ;; elts = list of at least n elements
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree     (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts) right-size)))
            (let ((right-tree     (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry 
                               left-tree 
                               right-tree)
                    remaining-elts))))))))

;; ex-2.62
(define (union-merge ol1 ol2)
  (cond ((null? ol1) ol2)
        ((null? ol2) ol1)
        ((= (car ol1) (car ol2)) (cons (car ol1)
                                       (union-merge (cdr ol1) (cdr ol2))))
        ((< (car ol1) (car ol2)) (cons (car ol1)
                                       (union-merge (cdr ol1) ol2)))
        ((> (car ol1) (car ol2)) (cons (car ol2)
                                       (union-merge ol1 (cdr ol2))))
        (else (error "!!?"))))

;; 2.3.3-Sets-as-ordered-lists
(define (intersection-merge ol1 ol2)
  (cond ((null? ol1) '())
        ((null? ol2) '())
        ((= (car ol1) (car ol2)) (cons (car ol1)
                                       (intersection-merge (cdr ol1) (cdr ol2))))
        ((< (car ol1) (car ol2)) (intersection-merge (cdr ol1) ol2))
        ((> (car ol1) (car ol2)) (intersection-merge ol1 (cdr ol2)))
        (else (error "!!?"))))

;; ex-2.65
(define (union-set set1 set2)
  (list->tree
    (union-merge (tree->list set1)
                 (tree->list set2))))
(define (intersection-set set1 set2)
  (list->tree
    (intersection-merge (tree->list set1)
                        (tree->list set2))))

;; demo

(define tree-2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree-2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree-2-16-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(union-set        tree-2-16-1 tree-2-16-2)
(union-set        tree-2-16-3 '(0 () (15 (13 () ()) (17 () ()))))
(newline)
(intersection-set tree-2-16-1 tree-2-16-2)
(intersection-set tree-2-16-3 '(5 (0 () ()) (7 () ())))
