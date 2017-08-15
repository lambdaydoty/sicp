#lang racket
(require racket/trace)



(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (cond ((and (variable? v1) (variable? v2)) (eq? v1 v2))
        (else #f)))

(define (make-sum     a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        (else (list '+ a1 a2))))
(define (addend sum) (cadr sum))
(define (augend sum) (caddr sum))
(define (sum? x)
  (cond ((null? x) #f)
        ((not (pair? x)) #f)
        (else (eq? (car x) '+))))

(define (make-product a1 a2) 
  (cond ((and (number? a1) (number? a2)) (* a1 a2))
        ((and (number? a1) (= a1 0)) 0)
        ((and (number? a2) (= a2 0)) 0)
        ((and (number? a1) (= a1 1)) a2)
        ((and (number? a2) (= a2 1)) a1)
        (else (list '* a1 a2))))
(define (multiplier   prod) (cadr prod))
(define (multiplicand prod) (caddr prod))
(define (product? x)
  (cond ((null? x) #f)
        ((not (pair? x)) #f)
        (else (eq? (car x) '*))))

(define (make-exponentiation base n)
  (cond ((and (number? base) (number? n)) (expt base n))
        ((and (number? n) (= n 0)) 1)
        ((and (number? n) (= n 1)) base)
        (else (list '** base n))))
(define (base pow) (cadr pow))
(define (exponent pow) (caddr pow))
(define (exponentiation? x)
  (cond ((null? x) #f)
        ((not (pair? x)) #f)
        (else (eq? (car x) '**))))


(define (deriv exp var)
  (cond ((number?   exp)  0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum?      exp) (make-sum (deriv (addend exp) var)
                                   (deriv (augend exp) var)))
        ((product?  exp) (make-sum
                           (make-product (multiplier   exp)
                                         (deriv (multiplicand exp) var))
                           (make-product (deriv (multiplier   exp) var)
                                         (multiplicand exp))))
        ((exponentiation? exp) (let ((a (base exp))                             
                                     (n (exponent exp)))                        
                                 (let ((first  n)                                
                                       (second (make-exponentiation a (- n 1))) 
                                       (third  (deriv a var)))     
                                   (make-product                   
                                     (make-product                  
                                       first second) third))))
        (else (error "unknown expression type: DERIV" exp))))

;; demo

(deriv '(+ x 3) 'x)
; (+ 1 0)
(deriv '(* x y) 'x)
; (+ (* x 0) (* 1 y))
(deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* (* x y) (+ 1 0))
;    (* (+ (* x 0) (* 1 y))
;       (+ x 3)))

(deriv '(** (+ (* 2 x) 1) 3) 'x)
(deriv '(** x 1) 'x)
(deriv '(* x x x) 'x)
(deriv '(+ x x 1 x x) 'x)
