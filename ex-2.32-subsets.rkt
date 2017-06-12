#lang racket
(require racket/trace)



(define (subsets s)
  (cond ((null? s) (list '()))
        (else (let ((rest (subsets (cdr s))))
                (append (map (lambda (y) (cons (car s) y)) rest)
                        rest)))))



(trace subsets)
(subsets '(1 2 3))

