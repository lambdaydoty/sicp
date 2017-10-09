#lang racket
(require "2.4.3-module-complex-package-ben.rkt")
(require "2.4.3-module-complex-package-alyssa.rkt")
(require "2.4.3-module-complex-arithmetics.rkt")
(require "2.4.3-module-complex-selectors-constructors.rkt")

;(trace put)
;(trace get)
(install-rectangular-package)
(install-polar-package)
(add-complex (make-from-real-imag 1 1)
             (make-from-mag-ang   2 (/ pi 2)))
(mul-complex (make-from-real-imag 1 1)
             (make-from-mag-ang   2 (/ pi 2)))
;(apply-generic 'no-such-tag)


;; ----------------------------------------------------------------------------
;; +---------------------+-----------------+-----------------------+
;; | _                   | Polar           | Rectangular           |
;; | -                   | -----           | -----------           |
;; | rea1-part           | real-part-polar | real-part-rectangular |
;; | imag-part           | imag-part-polar | imag-part-rectangular |
;; | magnitude           | magnitude-polar | magnitude-rectangular |
;; | angle               | angle-polar     | angle-rectangular     |
;; | make-from-real-imag | return '(r a)   | return '(x y)         |
;; | make-from-mag-ang   | return '(r a)   | return '(x y)         |
;; +---------------------+-----------------+-----------------------+

