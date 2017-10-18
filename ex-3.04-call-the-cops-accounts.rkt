#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (echo-message amount)
    "Incorrect password")
  (define (call-the-cops amount)
    "Cops are coming!")
  (let ((count 0))
    (define (dispatch check-pass m)
      (if (not (eq? password check-pass))
        (begin (set! count (+ count 1))
               (if (> count 7)
                 call-the-cops
                 echo-message))
        (begin (set! count 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit)  deposit)
                     (else (error "Unknown request -- MAKE-ACCOUNT" m))))))
    dispatch))

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'deposit) 1)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)