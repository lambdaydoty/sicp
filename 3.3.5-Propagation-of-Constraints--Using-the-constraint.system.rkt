#lang racket
(require "3.3.5-Propagation-of-Constraints--Representing-connectors.rkt")
(require "3.3.5-Propagation-of-Constraints--Implementing-the-constraint-system.rkt")

;; Create the network:
(define (celsius-fahrenheit-converter c f)
  (let ([u (make-connector)]
        [v (make-connector)]
        [w (make-connector)]
        [x (make-connector)]
        [y (make-connector)])
    (multiplier c w u)  ; 把 multiplier 加到 c w u 中
    (multiplier v x u)  ; 把 multiplier 加到 v x u 中
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)
(probe "Celsius temp" C)    ; 同樣地把 probe 加到 C 中
(probe "Fahrenheit temp" F) ; 同樣地把 probe 加到 F 中

;; demo
(display "---- demo ----")  (newline)
(set-value! C 25 'user)     (newline)
;(set-value! F 212 'user)
(forget-value! C 'user)     (newline)
(set-value! F 212 'user)    (newline)
(newline)
(newline)


; trace:
; (forget-value! C 'user)
; => (forget-my-value 'user)
; => (for-each-except retractor inform-about-value constraints)
;       => (probe 'I-lost-my-value)     => "probe-forget ..."
;       => (multiplier 'I-lost-my-value) => "multiplier-forget ..."
;           => (forget-value! product me-mult-left)
;           => (forget-my-value me-mult-left)
;           => (for-each-except retractor inform-about-value constraints)
;               => (multiplier 'I-lost-my-value) => "multiplier-forget ..."
;                   => (forget-value! product me-mult-center)  => 'ignored
;                   => (forget-value! m1      me-mult-center)
;                   => (forget-my-value me-mult-center)
;                   => (for-each-except retractor inform-about-value constraints)
;                   => (adder 'I-lost-my-value) => "adder-forget ..."
;                       => (forget-value! sum me-add-right)
;                       => (forget-my-value me-add-right)
;                       => (for-each-except retractor inform-about-value constraints)
;                       => (probe 'I-lost-my-value) => "probe-forget ..."
;                       => (forget-value! a1  me-add-right)
;                       => (forget-my-value   me-add-right)   => 'ignored
;                       => (forget-value! a2  me-add-right)
;                       => (forget-my-value   me-add-right)   => 'ignored
;                   => (forget-value! m2      me-mult-center) => 'ignored
;           => (forget-value! m1 me-mult-left) => 'ignored
;           => (forget-value! m2 me-mult-left) => 'ignored

;; test Z = X*Y
(define X (make-connector))
(define Y (make-connector))
(define Z (make-connector))
(multiplier X Y Z)
(probe "The mulitplier"   X)
(probe "The multiplicand" Y)
(probe "The product"      Z)
(set-value! X 3 'user)
(set-value! Y 0 'user)
(forget-value! X 'user)
