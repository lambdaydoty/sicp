#lang racket
(require racket/trace)
(define (square x) (* x x))
(define *op-table* (make-hash))
(define (put op type proc)  (hash-set! *op-table* (list op type) proc))
(define (get op type)       (hash-ref  *op-table* (list op type) #f)) ; replace '() by #f

;; 2.4.3

;; Manipulation of Tagged Data
(define (attach-tag type-tag 
                    contents) (cons type-tag contents))
(define (type-tag   datum   ) (if (pair? datum)
                                (car datum)
                                (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents   datum   ) (if (pair? datum)
                                (cdr datum)
                                (error "Bad tagged datum: CONTENTS" datum)))

;; Define our tag type for the Complex Number System
(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar?       z) (eq? (type-tag z) 'polar))

(define (add-complex z1 z2) (make-from-real-imag (+ (real-part z1) (real-part z2))
                                                 (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2) (make-from-real-imag (- (real-part z1) (real-part z2))
                                                 (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2) (make-from-mag-ang   (* (magnitude z1) (magnitude z2))
                                                 (+ (angle     z1) (angle     z2))))
(define (div-complex z1 z2) (make-from-mag-ang   (/ (magnitude z1) (magnitude z2))
                                                 (- (angle     z1) (angle     z2))))

;; /* -------------------------------------------------------------- *\
;;  *                 THE ABSTRACT BARRIER (generic)                 *
;; \* -------------------------------------------------------------- */

(define (real-part z) (apply-generic 'real-part z)) ;; -- The generic selectors --
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle     z) (apply-generic 'angle     z))
;; -- We construct :
;;      construct rectangular numbers whenever we have real and imaginary parts, and 
;;      polar numbers whenever we have magnitudes and angles.
(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang   r a) ((get 'make-from-mag-ang   'polar      ) r a))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC"
        (list op type-tags))))))

;; /* -------------------------------------------------------------- *\
;;  *                 THE ABSTRACT BARRIER                           *
;; \* -------------------------------------------------------------- */

(define (install-rectangular-package) ; Ben's representation (rectangle form)
  ;; internal procedures
  (define (real-part z)             (car z))
  (define (imag-part z)             (cdr z))
  (define (magnitude z)             (sqrt (+ (square (real-part z))
                                             (square (imag-part z)))))
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

(define (install-polar-package) ; Alyssa's representation (polar form)
  ;; internal procedures
  (define (magnitude z)             (car z))
  (define (angle     z)             (cdr z))
  (define (real-part z)             (* (magnitude z) (cos (angle z))))
  (define (imag-part z)             (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y)))
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

;; -------------------------------------------------------------------------
;; -------------------------------------------------------------------------

(trace put)
(trace get)
(install-rectangular-package)
(install-polar-package)

(add-complex (make-from-real-imag 1 1)
             (make-from-mag-ang   2 (/ pi 2)))
(mul-complex (make-from-real-imag 1 1)
             (make-from-mag-ang   2 (/ pi 2)))
(apply-generic 'no-such-tag)

