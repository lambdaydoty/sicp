#lang racket
(require "3.3.5-Propagation-of-Constraints--Representing-connectors.rkt")
(require "3.3.5-Propagation-of-Constraints--Implementing-the-constraint-system.rkt")
(define (square x) (* x x))

;; [a -- b]   
;; a^2 = b


;; Create the network:
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error "square less than 0 -- SQUARER" (get-value b))
        (set-value! a 
                    (sqrt (get-value b)) 
                    me))
      (if (has-value? a)
        (set-value! b 
                    (square (get-value a))
                    me)
        'nothing)))
        ;(error "both connectors have no value!"))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value)) 
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)


(define A (make-connector))
(define B (make-connector))
(squarer A B)
(probe "The square-root" A)
(probe "The square" B)

;; demo
(set-value! A 11 'user)
(forget-value! A 'user)
(set-value! B 169 'user)
;(set-value! B 20 'user)
;(forget-value! A 'user)
;(set-value! A 30 'user)
