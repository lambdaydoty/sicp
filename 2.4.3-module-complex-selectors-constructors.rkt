#lang racket
(require "2.4.3-module-put-get.rkt")
(require "2.4.3-module-tags.rkt")
(provide real-part)
(provide imag-part)
(provide magnitude)
(provide angle    )
(provide make-from-real-imag)
(provide make-from-mag-ang  )


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle     z) (apply-generic 'angle     z))

(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang   r a) ((get 'make-from-mag-ang   'polar      ) r a))

