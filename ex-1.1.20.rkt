#lang racket
(require racket/trace)
(require racket/promise)


(define (make-monitored f)
  (let ((count 0))
    (lambda args
      (cond ((eq? (car args) 'how-many-calls?) count)
            ((eq? (car args) 'reset-count) (set! count 0))
            (else (begin
                    (display "+1")
                    (set! count (+ 1 count))
                    (delay (f (force (car args))
                              (force (cadr args))))))))))


(define mem-remainder (make-monitored remainder))
;(define (gcd1 a b)
;  (if (= b 0)
;    a
;    (gcd1 b (mem-remainder a b))))
;(trace gcd1)
;(gcd1 206 40)
;(display "\n")
;(mem-remainder 'how-many-calls?)
;(mem-remainder 'reset-count)

(define (delayed-gcd a b)
  (if (= (if (promise? b) (force b) b) 0)
    (delay a)
    (delay (delayed-gcd b
                        (mem-remainder a b)))))
(trace delayed-gcd)
;(force
;  (force 
;    (force 
;      (force 
;        (force (delayed-gcd 206 40))))))
        (force (delayed-gcd (delay 206) (delay 40)))
(display "\n")
(mem-remainder 'how-many-calls?)
(mem-remainder 'reset-count)
