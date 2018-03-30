#lang racket
(require "3.3.4--Representing-wires.rkt")
(require "3.3.4--Implementing-the-agenda.rkt")
; -----------------------------
; 3.3.2-Representing-Queues.rkt
; -----------------------------
;  ^
;  |
; ----------------------------------          -----------------------------
; 3.3.4--Implementing-the-agenda.rkt          3.3.4--Representing-wires.rkt
; ----------------------------------          -----------------------------
;  ^                                           ^           
;  |                                           |           
; ---------------------                        |           
; The  agenda (OBJECT)                         |           
; ---------------------                        |           
;                ^                             |           
;                |                             |           
;               -----------------------------------   
;                      (and-gate) (inverter)         
;               -----------------------------------  
;               ------------------------------------   
;               ex-3.29-or-gate-by-and-gate-inverter  
;                              (or-gate)              
;               ------------------------------------  
;                                         ^
;                                         |
;                             --------------------------
;                                        DEMO
;                             --------------------------
(define the-agenda (make-agenda))
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ([first-item (first-agenda-item the-agenda)])
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))
; ---------------------------------------------------
(define inverter-delay 2)
(define and-gate-delay 3)
(define (inverter input output)
  (define (logical-not s)
    (cond ([= s 0] 1)
          ([= s 1] 0)
          (else (error "Invalid signal" s))))
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (and-gate a1 a2 output)
  (define (logical-and s t)
    (cond ((and [= s 0] [= t 0]) 0)
          ((and [= s 0] [= t 1]) 0)
          ((and [= s 1] [= t 0]) 0)
          ((and [= s 1] [= t 1]) 1)
          (else (error "Invalid signal" s t))))
  (define (and-action-procedure)
    (let ([new-value
            (logical-and (get-signal a1)
                         (get-signal a2))])
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
; ---------------------------------------------------
(define (or-gate a1 a2 output)
  (let ([b1 (make-wire)]
        [b2 (make-wire)]
        [c  (make-wire)])
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)
    'ok))
; --------------------------------------------------- DEMO
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))
(define a1  (make-wire))
(define a2  (make-wire))
(define out (make-wire))
(probe 'or-wire out)
(or-gate a1 a2 out)
(set-signal! a1 0)
(propagate)
(set-signal! a2 0)
(propagate)
