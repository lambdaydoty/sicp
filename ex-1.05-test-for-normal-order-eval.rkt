;#lang sicp

(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))

(test 0 (p))

; Racket support a NORMAL ORDER application by
; "Determine language from source" ->
; "Other Languages" ->
; "Experimental Languages" ->
; "Lazy Racket"
