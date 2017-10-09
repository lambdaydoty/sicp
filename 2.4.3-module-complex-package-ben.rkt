#lang racket
(require "2.4.3-module-tags.rkt")
(require "2.4.3-module-put-get.rkt")
(provide install-rectangular-package)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z)             (car z))
  (define (imag-part z)             (cdr z))
  (define (magnitude z)             (sqrt (+ (expt (real-part z) 2)
                                             (expt (imag-part z) 2))))
  (define (angle     z)             (atan (imag-part z)
                                          (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang   r a) (cons (* r (cos a))
                                          (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle     '(rectangular) angle    )
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'rectangular (lambda (x y) (tag (make-from-mag-ang   x y)))))

