#lang racket
(require racket/trace)

(define (reverse0 l)
  (cond ((null? l) '())
        (else (append (reverse0 (cdr l))
                      (list (car l))))))
(trace reverse0)
(reverse0 '(23 72 149 34))
(display "\n")


(define (reverse1 l)
  (define (iter left right)
    (if (null? left)
      right
      (iter (cdr left) (cons (car left) right))))
  (trace iter)
  (iter l '()))
(reverse1 '(23 72 149 34))
