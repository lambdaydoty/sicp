#lang sicp

(define (a-plus-abs-b a b)
  ((if (> b 0) + -)
   a
   b))

(a-plus-abs-b 0 3)
(a-plus-abs-b 0 2)
(a-plus-abs-b 0 1)
(a-plus-abs-b 0 0)
(a-plus-abs-b 0 -1)
(a-plus-abs-b 0 -2)
(a-plus-abs-b 0 -3)     ; a + |b|
