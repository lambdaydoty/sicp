#lang racket
(require racket/trace)
(define (square x) (* x x))
(define *op-table* (make-hash))
(define (put op type proc)  
  (hash-set! *op-table* (list op type) proc))
(define (get op type)       
  (hash-ref  *op-table* (list op type) #f)) ; replace '() by #f

;; [Q] A) More than one useful representation for a data object.
;;     B) Permission of different choices of representation to coexist
;;        
;; 2.4.1      Abstraction Barrier
;; 2.4.2 (*)  Tagged Data + Generic Operations
;; 2.4.3 

(define (add-complex z1 z2) 
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2) 
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2) 
  (make-from-mag-ang   (* (magnitude z1) (magnitude z2))
                       (+ (angle     z1) (angle     z2))))
(define (div-complex z1 z2) 
  (make-from-mag-ang   (/ (magnitude z1) (magnitude z2))
                       (- (angle     z1) (angle     z2))))

;; /* -------------------------------------------------------------- *\
;;  *                 THE ABSTRACT BARRIER (generic)                 *
;; \* -------------------------------------------------------------- */
;; SELECTORS
(define (real-part z) (cond ((rectangular? z) (real-part-rectangular (contents z))) 
							((polar?       z) (real-part-polar       (contents z)))
							(else             (error "Unknown type: REAL-PART" z))))
(define (imag-part z) (cond ((rectangular? z) (imag-part-rectangular (contents z)))
							((polar?       z) (imag-part-polar       (contents z)))
							(else             (error "Unknown type: IMAG-PART" z))))
(define (magnitude z) (cond ((rectangular? z) (magnitude-rectangular (contents z)))
							((polar?       z) (magnitude-polar       (contents z)))
							(else             (error "Unknown type: MAGNITUDE" z))))
(define (angle     z) (cond ((rectangular? z) (angle-rectangular     (contents z)))
							((polar?       z) (angle-polar           (contents z)))
							(else             (error "Unknown type: ANGLE"     z))))
;; CONSTRUCTORS
(define (make-from-real-imag x y) (make-from-real-imag-rectangular x y)) 
(define (make-from-mag-ang   r a) (make-from-mag-ang-polar         r a))








;; /* -------------------------------------------------------------- *\
;;  *               Manipulation of Tagged Data                      *
;; \* -------------------------------------------------------------- */
(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag   datum)             (if (pair? datum)
                                         (car datum)
                                         (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents   datum)             (if (pair? datum)
                                         (cdr datum)
                                         (error "Bad tagged datum: CONTENTS" datum)))
;; Define our tag type for the Complex Number System
(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar?       z) (eq? (type-tag z) 'polar))


;; /* -------------------------------------------------------------- *\
;;  *                 THE ABSTRACT BARRIER                           *
;; \* -------------------------------------------------------------- */

;; Ben's representation (rectangle form) :: Handle the name conflict
(define (real-part-rectangular z)             (car z))
(define (imag-part-rectangular z)             (cdr z))
(define (magnitude-rectangular z)             (sqrt (+ (square (real-part-rectangular z))
                                                       (square (imag-part-rectangular z)))))
(define (angle-rectangular     z)             (atan (imag-part-rectangular z)
                                                    (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y) (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular   r a) (attach-tag 'rectangular (cons (* r (cos a))
                                                                             (* r (sin a)))))










;; Alyssa's representation (polar form) :: Handle the name conflict
(define (magnitude-polar z)                   (car z))
(define (angle-polar     z)                   (cdr z))
(define (real-part-polar z)                   (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)                   (* (magnitude-polar z) (sin (angle-polar z))))
(define (make-from-real-imag-polar x y)       (attach-tag 'polar (cons (sqrt (+ (square x)
                                                                                (square y)))
                                                                       (atan (y x)))))
(define (make-from-mag-ang-polar   r a)       (attach-tag 'polar (cons r a)))









;; -------------------------------------------------------------------------
;; -------------------------------------------------------------------------





(add-complex (make-from-real-imag 1 1)
             (make-from-mag-ang   2 (/ pi 2)))
(mul-complex (make-from-real-imag 1 1)
             (make-from-mag-ang   2 (/ pi 2)))


