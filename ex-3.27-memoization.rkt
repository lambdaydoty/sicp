#lang racket
(require "3.3.3-Representing-Tables--1D-table.rkt")
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))



;; Memoization

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))


(newline)
(newline)
(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)
(fib 40)

(newline)

(define (memoize f)
  (let ([table (make-table)])
    (define lookup (table 'lookup-proc))
    (define insert! (table 'insert-proc!))
    (lambda (x)
      (let ([previously-computed-result (lookup x)])
        (if previously-computed-result
          previously-computed-result
          (let ([result (f x)])
            (insert! x result)
            result))))))
;        (or previously-computed-result
;            (let ([result (f x)])
;              (insert! x result table)
;              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(memo-fib 40)
