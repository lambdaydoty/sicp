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

(define my-set (adjoint-set 3
                            (adjoint-set 1
                                         (adjoint-set 4
                                                      (adjoint-set 1
                                                                   (adjoint-set 5
                                                                                (adjoint-set 9
                                                                                             (adjoint-set 2
                                                                                                          (adjoint-set 6
                                                                                                                       '())))))))))
(trace element-of-set?)
(element-of-set? 10 my-set)
(element-of-set? 5  my-set)
(element-of-set? 0  my-set)
