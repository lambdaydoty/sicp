#lang racket
(require "3.3.4--Representing-wires.rkt")
(require "3.3.4--Implementing-the-agenda.rkt")
(require racket/trace)
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
(define (check-agenda-detail)
  (print-agenda the-agenda))
; ---------------------------------------------------
(define and-gate-delay 3)
(define (and-gate a1 a2 output)
  (define (logical-and s t)
    (cond ((and [= s 0] [= t 0]) 0)
          ((and [= s 0] [= t 1]) 0)
          ((and [= s 1] [= t 0]) 0)
          ((and [= s 1] [= t 1]) 1)
          (else (error "Invalid signal" s t))))
  (define (and-action-procedure)    ; #p:and
    (let ([new-value
            (logical-and (get-signal a1)
                         (get-signal a2))])
      (define (set-output!) (set-signal! output new-value)) ; #p:set-out
      (trace set-output!)
      (after-delay and-gate-delay set-output!)))
                   ;(lambda () (set-signal! output new-value))))) 
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
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
(define a1  (make-wire))    ; a1:{ 0, ()}
(define a2  (make-wire))    ; a1:{ 0, ()}        a2:{ 0, ()}
(define out (make-wire))    ; a1:{ 0, ()}        a2:{ 0, ()}        out:{ 0, ()}
(probe 'and-wire out)       ; a1:{ 0, ()}        a2:{ 0, ()}        out:{ 0, (#p:probe)}  => (#p:prob)
(check-agenda-detail)
(and-gate a1 a2 out)        ; a1:{ 0, (#p:and)}  a2:{ 0, (#p:and)}  out:{ 0, (#p:probe)}  3.[#p:set-out #p:set-out]
(check-agenda-detail)


(set-signal! a1 0)          ; a1:{ 0, (#p:and)}  a2:{ 0, (#p:and)}  out:{ 0, (#p:probe)}  3.[#p:set-out #p:set-out]             => 'done
(set-signal! a2 1)          ; a1:{ 0, (#p:and)}  a2:{ 1, (#p:and)}  out:{ 0, (#p:probe)}  3.[#p:set-out #p:set-out #p:set-out]  => (#p:and)
(check-agenda-detail)

(propagate)                 ; => (#p:set-out) (#p:set-out) (#p:set-out)
(check-agenda-detail)


(set-signal! a1 1)          ; a1:{ 1, (#p:and)}  a2:{ 1, (#p:and)}  out:{ 0, (#p:probe)}  6.[#p:set-out]                        => (#p:and)
(set-signal! a2 0)          ; a1:{ 1, (#p:and)}  a2:{ 0, (#p:and)}  out:{ 0, (#p:probe)}  6.[#p:set-out #p:set-out]             => (#p:and)
(check-agenda-detail)
(propagate)                 ; => (#p:set-out) (#p:set-out)          out->1->0
