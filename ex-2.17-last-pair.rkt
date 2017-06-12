#lang racket
(require racket/trace)

(define (last-pair l)
  (cond ((null? l) (display "Not a nonempty list!\n"))
        ((null? (cdr l)) l)
        (else (last-pair (cdr l)))))

(last-pair '(23 72 149 34))
(last-pair '(23 72 149))
(last-pair '(23 72))
(last-pair '(23))
(last-pair '())

(display "\n")
(trace last-pair)
(last-pair '(23 72 149 34))

