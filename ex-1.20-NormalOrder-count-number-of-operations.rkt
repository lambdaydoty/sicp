#lang racket
(require racket/trace)
;; Euclid's Algorithm

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(trace gcd)
(gcd 206 40)

;; (gcd 206 40)
;; (gcd 40 6)
;; (gcd 6 4)
;; (gcd 4 2)
;; (gcd 2 0)
;; Normal-order Evaluation:
;; 	1) Substitute operand expressions for parameters
;; 	2) until it obtained an expression involving only primitive operators,
;; 	3) and would then perform the evaluation
;; The evaluation rule for the special form ``if''
;; 	1) The predicate expression is evaluated first, 
;; 	2) and the result determines whether to evaluate the consequent
;; 	3) or the alternative expression.
;; (gcd 206                                                                              40)      %:0
;; (gcd 40                                                                        (% 206 40))     %:1
;; (gcd (% 206 40)                                                          (% 40 (% 206 40)))    %:2
;; (gcd (% 40 (% 206 40))                                     (% (% 206 40) (% 40 (% 206 40))))   %:4
;; (gcd (% (% 206 40) (% 40 (% 206 40))) (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40)))))  %:7
;;                                                                                        (return)%:4
;;                                                                                        -----------
;;                                                                                                 18



