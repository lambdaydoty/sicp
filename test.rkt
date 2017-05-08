#lang sicp

486             ; eval a primitive expression -- a numeral
(display "\n")

(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)      ; eval combinations (with operator being primitive procedures)
(display "\n")  ; apply a primitive procedure --> interpreter-builtin
                ; applicative order:
                ;       1. eval the operator
                ;       2. eval the operands
                ;       3. apply the resulting proc to the resulting argv

(+ 21 35 12 7)  
(* 25 4 12)     
(+ (* 3 5)
   (- 10 6))    
(+ (* 3 
      (+ (* 2 4) 
         (+ 3 5))) 
   (+ (- 10 7) 
      6))       ; benefits of the prefix notation
(display "\n")

(* (+ 2 
      (* 4 
         6)) 
   (+ 3 
      5 
      7))       ; eval combinations (nested) - tree accumulation
(display "\n")

(define x 3)    ; eval the DEFINE special form
(display "\n")

(define (square x) (* x x))
(display "\n")  ;  ^^^^^^^
                ; eval the DEFINE special form for a compound procedure

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)           ; eval a combination whose operator is a compound procedure
(display "\n")  ; apply a compound procedure --> eval the BODY of the procedure
                ;                                with each formal prameter
                ;                                replaced by the corresponding
                ;                                argument
                ; [the SUBSTITUTION model for proc application]
                ; the applicative order vs.
                ; the normal order:
                ; 1. eval the operator
                ; 2. substitute operand expressions for parameters
                ; 3. until an expression involving only primitive operators
                ;    and then perform the eval.

(define (p) 
  (p))
(define (test x y)
  (if (= x 0)
    0
    y))
(test 0 (p))

; (test 0 (p))  [in applicative order]
; => (test 0 (p))
; => (test 0 (p))
; => (test 0 (p))
; => ...

; (test 0 (p))  [in normal order]
; => (if (= 0 0)
;       0
;       (p))
; => 0





