#lang racket
(require racket/trace)

(define (reverse-recur l)
  (cond ((null? l) '())
        (else (append (reverse-recur (cdr l))
                      (list (car l))))))
(define (reverse-iter l)
  (define (iter left right)
    (if (null? left)
      right
      (iter (cdr left) (cons (car left) right))))
  (iter l '()))

(define (deep-reverse-recur items)
  (cond ((not (pair? items)) items)
        (else (append (deep-reverse-recur (cdr items))
                      (list (deep-reverse-recur (car items)))))))

(define (deep-reverse-iter items)
  (define (iter left right)
    (if (null? left)
      right
      (iter (cdr left) (cons (deep-reverse-iter (car left)) right))))
  (cond ((not (pair? items)) items)
        (else (iter items '()))))

(define (deep-reverse-map items)
  (if (not (pair? items))
    items
    (reverse (map deep-reverse-map items))))

(define (deep-reverse-map-better items)
  (reverse (map (lambda (i)
                  (if (not (pair? i))
                    i
                    (deep-reverse-map-better i))) items)))

;  (define (reverse items)
;    (if (not (pair? items))
;      items
;      (reverse-recur items)))
;  (reverse (map reverse items)))

(define (demo reverse)
  (define x '((1 2) (3 4)))
  (define y '(((( 1 ) 2 ) 3 4) 5 6 7))
  (define z '((a b) (c d) (e f)))
  (display (reverse x))
  (display "  ")
  (display (reverse y))
  (display "  ")
  (display (reverse z))
  (display "\n"))

;(trace deep-reverse-map-better)
(demo reverse-recur)
(demo reverse-iter)
(demo deep-reverse-recur)
(demo deep-reverse-iter)
(demo deep-reverse-map)
(demo deep-reverse-map-better)
