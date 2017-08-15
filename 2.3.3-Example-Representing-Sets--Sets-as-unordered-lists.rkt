#lang racket
(require racket/trace)

(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)                      ; nil + nil
        ((or  (null? l1) (null? l2)) #f)                      ; nil + not nil
        ((and (not (pair? l1)) (not (pair? l2))) (eq? l1 l2)) ; symbol + symbol
        ((or  (not (pair? l1)) (not (pair? l2))) #f)          ; symbol + non-nil list
        (else (and (equal? (car l1) (car l2))                 ; non-nil lists
                   (equal? (cdr l1) (cdr l2))))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoint-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))
(define (intersection-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((or  (null? set1) (null? set2)) '())
        (else (if (element-of-set? (car set1) set2) 
          (cons (car set1) (intersection-set (cdr set1) set2))
          (intersection-set (cdr set1) set2)))))

;(define A (adjoint-set 'x '(p q r s)))
;(element-of-set? 'x A)
;(element-of-set? 'x '(p q r x s))
;(element-of-set? 'x '(p q r s))
;(element-of-set? 'x '())
;(adjoint-set 'x '())
;(adjoint-set 'x (adjoint-set 'x '()))
;(adjoint-set 'y (adjoint-set 'x (adjoint-set 'x '())))
;(intersection-set '(x y z) '(p q x a b z))
;(intersection-set '(x y z) '(p q a b c))
