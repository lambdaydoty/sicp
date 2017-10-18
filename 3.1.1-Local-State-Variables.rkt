#lang racket

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 25)
(withdraw 25)
(withdraw 60)
(withdraw 15)

(newline)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(new-withdraw 25) ; <== Observe that the expression (withdraw 25), evaluated twice
(new-withdraw 25) ; <== yields different values.
(new-withdraw 60)
(new-withdraw 15)

(newline)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100)) ; Observe that W1 and W2 are completely independent
(define W2 (make-withdraw 100)) ; objects, each with its own local state variable balance.
(W1 50)
(W2 70)
(W2 40)
(W1 40)

(newline)

(define (make-account balance)  ; message-passing
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit)  deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))
(define acc2 (make-account 100))
acc
((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit ) 40)
((acc 'deposit ) 40)
((acc 'withdraw) 60)
((acc2 'withdraw) 1)

; Another call to make-account will produce a 
; completely separate account object, which
; maintains its own local balance.
