#lang racket

(define f
  (let ((state 1))
    (lambda (n)
      (begin (set! state (- state n))
             state))))
  
(+ (f 0) (f 1))     ; (1-0) + (1-0-1) = 1
;(+ (f 1) (f 0))    ; (1-1) + (1-1-0) = 0


;Another solution:
#|
(define f
  (let ((count 0))
    (lambda (n)
      (set! count (- 1 count)) ; count jumps between 1 & 0
      (cond ((= count 1) 0) ; return 0 for the very first time
            ((= count 0) n) ; return the arguemnt for the rest call
            (else -111)))))
|#

