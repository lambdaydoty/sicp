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

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((= kinds-of-coins 0) 0) 
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (count-change amount)
  (cc amount 5))

(count-change 100)
(trace cc)
(count-change 11)

