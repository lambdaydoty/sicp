#lang racket
(require "2.4.3-module-put-get.rkt")
(require "2.4.3-module-tags.rkt")
(provide add)
(provide sub)
(provide mul)
(provide div)
(provide make-complex-from-real-imag)
(provide make-complex-from-mag-ang)
(provide make-rational)
(provide make-scheme-number)
(provide magnitude)


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex)       x y))
(define (make-complex-from-mag-ang   r a) ((get 'make-from-mag-ang   'complex)       r a))
(define (make-rational               n d) ((get 'make                'rational)      n d))
(define (make-scheme-number          n  ) ((get 'make                'scheme-number) n  ))
(define (magnitude x) (apply-generic 'magnitude x)) ; ex-2.77
