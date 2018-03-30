#lang racket
(require racket/trace)

;; Information Retrieval
;; lookup using unordered list

(define empty-set '())
(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)                      ; nil + nil
        ((or  (null? l1) (null? l2)) #f)                      ; nil + not nil
        ((and (not (pair? l1)) (not (pair? l2))) (eq? l1 l2)) ; symbol + symbol
        ((or  (not (pair? l1)) (not (pair? l2))) #f)          ; symbol + non-nil list
        (else (and (equal? (car l1) (car l2))                 ; non-nil lists
                   (equal? (cdr l1) (cdr l2))))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoint-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key
                 (key (car set-of-records))) (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (key a-record) (car a-record)) ; a record is implemented as a pair of (key . value)

(define DB (adjoint-set '(6 . value-6)
                       (adjoint-set '(8 . value-8)
                                    (adjoint-set '(1 . value-1)
                                                 (adjoint-set '(7 . value-7) empty-set)))))
DB
(newline)
(lookup 8 DB)
(lookup 0 DB)

