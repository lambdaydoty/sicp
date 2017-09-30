#lang racket
(require racket/trace)

(define (reverse-append lat)
  (cond ((null? lat) '())
        (else (append (reverse-append (cdr lat))
                      (list (car lat))))))
(trace reverse-append)


(define (reverse-iter l)
  (define (iter left right)
    (if (null? left)
      right
      (iter (cdr left) (cons (car left) right))))
  (trace iter)
  (iter l '()))

;; demo

(define test0 '())
(define test1 '(23 72 149 34))

(define (demo reverse)
  (reverse test0)
  (reverse test1)
  (display "\n"))

(demo reverse-append)
(demo reverse-iter)
