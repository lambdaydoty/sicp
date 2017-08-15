#lang racket
(require racket/trace)


;;; not finished !!


(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (cond ((and (variable? v1) (variable? v2)) (eq? v1 v2))
        (else #f)))
(define (make-sum     a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        (else (list a1 '+ a2))))    ;
(define (make-product a1 a2) 
  (cond ((and (number? a1) (number? a2)) (* a1 a2))
        ((and (number? a1) (= a1 0)) 0)
        ((and (number? a2) (= a2 0)) 0)
        ((and (number? a1) (= a1 1)) a2)
        ((and (number? a2) (= a2 1)) a1)
        (else (list a1 '* a2))))    ;
(define (sum? x)                    ; x * y + 7
  (cond ((null? x) #f)
        ((not (pair? x)) #f)
        (else (eq? (cadr x) '+))))  ;
(define (product? x)
  (cond ((null? x) #f)
        ((not (pair? x)) #f)
        (else (eq? (cadr x) '*))))  ;
(define (addend sum) (car sum))     ;
(define (augend sum) (caddr sum))
(define (multiplier   prod) (car prod)) ;
(define (multiplicand prod) (caddr prod))

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
        (else (error "unknown expression type: DERIV" exp))))

;; demo

; (x + 3 * (x + y + 2))
; x * y + 7
;
(deriv '(x + (3 * (x + (y + 2)))) 'x)

