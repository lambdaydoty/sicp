#lang racket
(require racket/trace)

(define (power t n)
  (cond ((= n 0) 1)
        (else (* t (power t (- n 1))))))
(define (extract-2-log x)
  (cond ((not (= (remainder x 2) 0)) 0)
        (else (+ 1 (extract-2-log (/ x 2))))))
(define (extract-3-log x)
  (cond ((not (= (remainder x 3) 0)) 0)
        (else (+ 1 (extract-3-log (/ x 3))))))


(define (cons x y)
  (* (power 2 x)
     (power 3 y)))


(define (car z)
  (extract-2-log z))

(define (cdr z)
  (extract-3-log z))

(trace cons)
(trace car)
(trace cdr)


(car (cons 1 2))
(cdr (cons 1 2))
(car (cons 0 2))
(cdr (cons 0 2))
