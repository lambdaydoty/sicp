#lang racket
(require racket/trace)
(define (square x) (* x x))



(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items '()))

(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items '()))

(define (square-list5 items)                ; the correct way; using `append'
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (append answer
                  (list (square (car things)))))))
  (iter items '()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square-list1 items)
  (if (null? items)
    '()
    (cons (square (car items))
          (square-list1 (cdr items)))))

(define (square-list2 items)
  (map square items))

(define (demo square-list)
  (begin
    (display (square-list '(1 2 3 4)))   (display "\n")))

(demo square-list1)
(demo square-list2)
(demo square-list3)
(demo square-list4)
(demo square-list5)
