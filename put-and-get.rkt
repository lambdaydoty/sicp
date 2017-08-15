#lang racket

(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) '()))

;;


(put 'square 'num (lambda (x) (* x x)))
((get 'square 'num) 3)
