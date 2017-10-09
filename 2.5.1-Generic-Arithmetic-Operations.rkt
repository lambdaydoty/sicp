#lang racket
(require racket/trace)
(define (square x) (* x x))
(define *op-table* (make-hash))
(define (put op type proc)  
  (hash-set! *op-table* (list op type) proc))
(define (get op type)       
  (hash-ref  *op-table* (list op type) #f)) ; replace '() by #f
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC"
        (list op type-tags))))))
(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag   datum)             (if (pair? datum)
                                         (car datum)
                                         (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents   datum)             (if (pair? datum)
                                         (cdr datum)
                                         (error "Bad tagged datum: CONTENTS" datum)))
;; 2.5.4 (*)  Generic Arithmetic Operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; /* -------------------------------------------------------------- *\
;;  *                 THE ABSTRACT BARRIER (top level)
;; \* -------------------------------------------------------------- */

;; A) COMPLEX
(define (install-complex-package)
  ;;imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang   r a) ((get 'make-from-mag-ang   'polar      ) r a))
  ;;internal procedures
  (define (add-complex z1 z2) (make-from-real-imag (+ (real-part z1) (real-part z2))
                                                   (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2) (make-from-real-imag (- (real-part z1) (real-part z2))
                                                   (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2) (make-from-mag-ang   (* (magnitude z1) (magnitude z2))
                                                   (+ (angle     z1) (angle     z2))))
  (define (div-complex z1 z2) (make-from-mag-ang   (/ (magnitude z1) (magnitude z2))
                                                   (- (angle     z1) (angle     z2))))
  ;;interface to rest of the system (new)
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'complex (lambda (r a) (tag (make-from-mag-ang   r a))))
  (put 'real-part '(complex) real-part) ; ex-2.77
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle     '(complex) angle    )
  'done)

;; B) RATIONALS
(define (install-rational-package)
  ;;internal procedures
  (define (make-rat n d)  (let ((g (gcd n d))) (cons (/ n g) (/ d g))))
  (define (numer z)       (car z))
  (define (denom z)       (cdr z))
  (define (add-rat x y)   (make-rat (+ (* (numer x) (denom y))
                                     (* (numer y) (denom x)))
                                    (* (denom x) (denom y))))
  (define (sub-rat x y)   (make-rat (- (* (numer x) (denom y)
                                          (numer y) (denom x)))
                                    (* (denom x) (denom y))))
  (define (mul-rat x y)   (make-rat (* (numer x) (numer y))
                                    (* (denom x) (denom y))))
  (define (div-rat x y)   (make-rat (* (numer x) (denom y))
                                    (* (denom x) (numer y))))
  ;(define (equal-rat? x y) (= (* (numer x) (denom y))
  ;                            (* (numer y) (denom x))))
  ;;interface to rest of the system (new)
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

;; C) SCHEME-NUMBERS
(define (install-scheme-number-package)
  ;;interface to rest of the system (new)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)
  

;; /* -------------------------------------------------------------- *\
;;  *                 THE ABSTRACT BARRIER (complex)                 *
;; \* -------------------------------------------------------------- */
;; SELECTORS
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle     z) (apply-generic 'angle     z))
;; CONSTRUCTORS

;; /* -------------------------------------------------------------- *\
;;  *                 THE ABSTRACT BARRIER                           *
;; \* -------------------------------------------------------------- */

(define (install-rectangular-package) ; Ben's
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

(define (install-polar-package) ; Alyssa's
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

(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex)       x y))
(define (make-complex-from-mag-ang   r a) ((get 'make-from-mag-ang   'complex)       r a))
(define (make-rational               n d) ((get 'make                'rational)      n d))
(define (make-scheme-number          n  ) ((get 'make                'scheme-number) n  ))
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)
(newline)
(define z (make-complex-from-real-imag 1 1       ))
(define w (make-complex-from-mag-ang   2 (/ pi 2)))
(define x (make-rational 1 3))
(define y (make-rational 1 2))
(define a (make-scheme-number 11))
(define b (make-scheme-number 22))
(add z w)
(mul z w)
(add x y)
(mul x y)
(add a b)
(mul a b)
(newline)
(newline)
(trace get)
(magnitude z)
;(add-complex (make-from-real-imag 1 1)
;             (make-from-mag-ang   2 (/ pi 2)))
;(mul-complex (make-from-real-imag 1 1)
;             (make-from-mag-ang   2 (/ pi 2)))
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

