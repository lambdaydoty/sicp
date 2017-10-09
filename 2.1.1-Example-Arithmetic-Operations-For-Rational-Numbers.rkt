#lang racket
;(define (make-rat n d)  (let ((g (gcd n d))) (cons (/ n g) (/ d g))))
;(define (numer z)       (car z))
;(define (denom z)       (cdr z))
;(define (print-rat z)
;  (newline)
;  (display (numer z))
;  (display "/")
;  (display (denom z)))
(require "ex-2.01-better-make-rat.rkt")
; Suppose we want to do arithmetic with rational numbers:
;   1) add
;   2) subtract
;   3) multiply
;   4) divide
;   5) test for equality
; Assume we already have a way of constructing and extracting procedures:
;   (make-rat <n> <d>)          (the constructors)      (the selectors)
;   (number <x>)
;   (denom  <x>)
;;
(define (add-rat x y)   (make-rat (+ (* (numer x) (denom y))
                                     (* (numer y) (denom x)))
                                  (* (denom x) (denom y))))
(define (sub-rat x y)   (make-rat (- (* (numer x) (denom y)
                                        (numer y) (denom x)))
                                  (* (denom x) (denom y))))
(define (mul-rat x y)   (make-rat (* (numer x) (numer y))
                                  (* (denom x) (denom y))))
(define (div-rat x y)   (make-rat (* (numer x) (denom y))
                                  (* (denom x) (numer y))))
(define (equal-rat? x y) (= (* (numer x) (denom y))
                            (* (numer y) (denom x))))
;;;;
(define one-half  (make-rat 1 2))
(define one-third (make-rat 1 3))
(print-rat one-half)
(print-rat (add-rat one-half  one-third))
(print-rat (mul-rat one-half  one-third))
(print-rat (add-rat one-third one-third))
(newline)
(print-rat (make-rat 7 -3))
(print-rat (make-rat -7 3))
(print-rat (make-rat -7 -3))


