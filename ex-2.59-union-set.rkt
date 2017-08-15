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
(define (intersection-set set1 set2)            ; recurive on |set1| is enough
  (cond ((null? set1) '())
        (else (if (element-of-set? (car set1) set2) 
                (cons (car set1) (intersection-set (cdr set1) set2))
                (intersection-set (cdr set1) set2)))))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (if (element-of-set? (car set1) set2)
                (union-set (cdr set1) set2)
                (cons (car set1) (union-set (cdr set1) set2))))))




(intersection-set '(x y z) '(p q x a b z))
(intersection-set '(p q x a b z) '(x y z))
(intersection-set '(x y z) '(p q a b c))
(intersection-set '(p q a b c) '(x y z))
(newline)
(union-set '(x y z) '(p q x a b z))
(union-set '(p q x a b z) '(x y z))
(union-set '(x y z) '(p q a b c))
(union-set '(p q a b c) '(x y z))
(union-set '(p q a b c) '())
(union-set '() '(p q a b c))
(newline)

;; implement by accumulation!
(define (intersection-set1 set1 set2)
  (foldr cons '()  (filter (lambda (x) (element-of-set? x set2))       set1)))
(define (union-set1 set1 set2)
  (foldr cons set2 (filter (lambda (x) (not (element-of-set? x set2))) set1)))

(intersection-set1 '(x y z) '(p q x a b z))
(intersection-set1 '(p q x a b z) '(x y z))
(intersection-set1 '(x y z) '(p q a b c))
(intersection-set1 '(p q a b c) '(x y z))
(newline)
(union-set1 '(x y z) '(p q x a b z))
(union-set1 '(p q x a b z) '(x y z))
(union-set1 '(x y z) '(p q a b c))
(union-set1 '(p q a b c) '(x y z))
(union-set1 '(p q a b c) '())
(union-set1 '() '(p q a b c))
