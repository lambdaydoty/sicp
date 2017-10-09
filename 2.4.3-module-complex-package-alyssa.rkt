#lang racket
(require "2.4.3-module-tags.rkt")
(require "2.4.3-module-put-get.rkt")
(provide install-polar-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z)             (car z))
  (define (angle     z)             (cdr z))
  (define (real-part z)             (* (magnitude z) (cos (angle z))))
  (define (imag-part z)             (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) (cons (sqrt (+ (expt x 2) (expt y 2)))
                                          (atan (y x))))
  (define (make-from-mag-ang   r a) (cons r a))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle     '(polar) angle    )
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'polar (lambda (x y) (tag (make-from-mag-ang   x y)))))

