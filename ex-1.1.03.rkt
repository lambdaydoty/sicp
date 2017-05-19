#lang sicp
;#lang racket                           ; if you need to trace the procedure
;(require racket/trace)

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))

(define (proc x y z)
  (cond ((> x y) (proc y x z))          ; exchange the value of x and y
        ((> y z) (proc x z y))          ; exchange the value of y and z
        (else (sum-of-squares y z))))   ; now we are promised that x <= y <= z

;(trace proc)                           ; if you need to trace the procedure
(proc 1 2 3)
(proc 1 3 2)
(proc 2 1 3)
(proc 2 3 1)
(proc 3 1 2)
(proc 3 2 1)


