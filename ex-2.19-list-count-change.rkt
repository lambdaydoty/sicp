#lang racket
(require racket/trace)


;; (1)   (5)  (10)  (25)  (50)
;; ------------------+
;;                   |
;;                (n = 4)
;;                   |
;;                   v
;;  first-denomination = 25

;; +------------------+-----+---+---+---+---+---+---+---+---+---+---+----+----+
;; |      amount      | ... | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 |
;; +------------------+-----+---+---+---+---+---+---+---+---+---+---+----+----+
;; | kinds-of-coins=0 |  0  | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0  | 0  |
;; | (1)              |  0  | 1 |   |   |   |   |   |   |   |   |   |    | D  |
;; | (1 5)            |  0  | 1 | C |   |   |   |   | D'|   |   |   |    | B  |
;; | (1 5 10)         |  C' | 1 | B'|   |   |   |   |   |   |   |   |    | A  |
;; | (1 5 10 25)      |  0  | 1 |   |   |   |   |   |   |   |   |   |    |    |
;; | (1 5 10 25 50)   |  0  | 1 |   |   |   |   |   |   |   |   |   |    |    |
;; +------------------+-----+---+---+---+---+---+---+---+---+---+---+----+----+
;;
;;      B   (D'+ D)
;; A =  +  =   +   = ...
;;      B'  (C'+ D)

(define (first-denomination       coin-values) (car coin-values))
(define (excep-first-denomination coin-values) (cdr coin-values))
(define (no-more?                 coin-values) (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((no-more? coin-values) 0) ; ((= kinds-of-coins 0) 0) 
        (else (+ (cc amount (excep-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values)))))
;        (else (+ (cc amount (- kinds-of-coins 1))
;                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))
(cc 100 us-coins)
(cc 100 (reverse us-coins))
(trace cc)
(cc 11 us-coins)
(cc 11 (reverse us-coins))

