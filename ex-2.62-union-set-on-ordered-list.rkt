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
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))
(define (adjoint-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoint-set x (cdr set))))))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        (else (let ((x1 (car set1))
                    (x2 (car set2))
                    (sub1 (cdr set1))
                    (sub2 (cdr set2)))
                (cond ((= x1 x2) (cons x1 (intersection-set sub1 sub2)))
                      ((< x1 x2) (intersection-set sub1 set2))
                      ((> x1 x2) (intersection-set set1 sub2)))))))
(define (union-set set1 set2)  ;; <-------------------------------
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2))
                    (sub1 (cdr set1))
                    (sub2 (cdr set2)))
                (cond ((= x1 x2) (cons x1 (union-set sub1 sub2)))
                      ((< x1 x2) (cons x1 (union-set sub1 set2)))
                      ((> x1 x2) (cons x2 (union-set set1 sub2))))))))

(trace element-of-set?)
(trace intersection-set)
(trace adjoint-set)
(trace union-set)
(union-set '(1 3 5 7) '(0 2 4 6 8))
(union-set '(0 2 4 6 8) '(1 3 5 7))
(union-set '(1 3 5 7) '(1 2 3 5 8 13))