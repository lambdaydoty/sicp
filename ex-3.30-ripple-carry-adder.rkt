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
;                 (and-gate) (or-gate) (inverter)         
;               -----------------------------------  
;                         half-adder
;               -----------------------------------  
;                         full-adder
;               -----------------------------------  
;                         ripple-carry-adder
;               -----------------------------------  
;                                ^
;                                |
;                    --------------------------
;                               DEMO
;                    --------------------------
; ---- The agenda object ----
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
; ----- Primitive function boxes ----
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
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
(define (or-gate a1 a2 output)
  (define (logical-or s t)
    (cond ((and [= s 0] [= t 0]) 0)
          ((and [= s 0] [= t 1]) 1)
          ((and [= s 1] [= t 0]) 1)
          ((and [= s 1] [= t 1]) 1)
          (else (error "Invalid signal" s t))))
  (define (or-action-procedure)
    (let ([new-value
            (logical-or (get-signal a1)
                        (get-signal a2))])
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
; ---- half-adder ----
(define (half-adder a b s c)
  (let ([d (make-wire)]
        [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
; ---- full-adder ----
(define (full-adder a b c-in sum c-out)
  (let ([s  (make-wire)]
        [c1 (make-wire)]
        [c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
; ---- ex-3.30 The ripple carry adder ----
(define (ripple-carry-adder a_k b_k s_k c)
  (define (recur ak bk sk c-out)
    (let ([c-in (make-wire)])
      (cond ((or (null? ak)
                 (null? bk)
                 (null? sk)) (set-signal! c-out 0))
            (else (recur (cdr ak) (cdr bk) (cdr sk) c-in)
                  (full-adder (car ak) 
                              (car bk)
                              c-in
                              (car sk)
                              c-out)))))
  (recur a_k b_k s_k c)
  'ok)
; ---- DEMO ----
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))
(define A (list (make-wire) (make-wire) (make-wire) (make-wire))) ; A0 A1 A2 A3
(define B (list (make-wire) (make-wire) (make-wire) (make-wire))) ; B0 B1 B2 B3
(define S (list (make-wire) (make-wire) (make-wire) (make-wire))) ; S0 S1 S2 S3
(define c (make-wire))
(probe 'sum-bit2 (list-ref S 0))
(probe 'sum-bit1 (list-ref S 1))
(probe 'sum-bit0 (list-ref S 2))
(probe 'sum-carry c)

(ripple-carry-adder A B S c)

(set-signal! (list-ref A 0) 1) ; The most significant bit
(set-signal! (list-ref A 1) 1)
(set-signal! (list-ref A 2) 1)
(set-signal! (list-ref B 0) 1) ; The most significant bit
(set-signal! (list-ref B 1) 1)
(set-signal! (list-ref B 2) 0)
(propagate)

