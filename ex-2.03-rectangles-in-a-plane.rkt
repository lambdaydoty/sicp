#lang racket
(require "ex-2.02-line-segments-in-a-plane.rkt")

;;; Implement a representation for rectangles in a plane ;;;
; #proc : compute the perimeter of a given rectangle.
; #proc : compute the area of a given rectangle.

; Implement A
; (define (make-rect seg1 seg2)   (cons seg1 seg2))
; (define (seg1-rect rect)        (car rect))
; (define (seg2-rect rect)        (cdr rect))

; Implement B
(define (make-rect pt0 pt1 pt2 pt3) (list pt0 pt1 pt2 pt3))
(define (pt0-rect rect) (car rect))
(define (pt1-rect rect) (cadr rect))
(define (pt2-rect rect) (caddr rect))
(define (pt3-rect rect) (cadddr rect))

;;; ---------------------------------------------------------
;;; ---------------- Abstraction Barrier --------------------
;;; ---------------- #proc make-rect     --------------------
;;; ---------------- #proc seg1-rect     --------------------
;;; ---------------- #proc seg2-rect     --------------------
;;; ---------------------------------------------------------

; Implement A
; (define (len-rect rect)
;   (len-segment
;     (seg1-rect rect)))
; (define (wid-rect rect)
;   (len-segment
;     (seg2-rect rect)))

; Implement B
(define (len-rect rect)
  (len-segment
    (make-segment (pt0-rect rect)
                  (pt1-rect rect))))
(define (wid-rect rect)
  (len-segment
    (make-segment (pt1-rect rect)
                  (pt2-rect rect))))

;;; ---------------------------------------------------------
;;; ---------------- Abstraction Barrier --------------------
;;; -----------------#proc len-rect rect --------------------
;;; -----------------#proc wid-rect rect --------------------
;;; ---------------------------------------------------------

(define (perimeter rect)
  (let ((len (len-rect rect))
        (wid (wid-rect rect)))
    (* 2 (+ len wid))))
(define (area rect)
  (let ((len (len-rect rect))
        (wid (wid-rect rect)))
    (* len wid)))

;;; ---------------------------------------------------------
;;; ---------------------------------------------------------

(define A (make-point 0 0))
(define B (make-point 3 4))
(define C (make-point 0 0))
(define D (make-point 5 12))

; Implement A
; (define segment-1 (make-segment A B))
; (define segment-2 (make-segment C D))
; (define my-rect   (make-rect segment-1 segment-2))
; (perimeter my-rect)
; (area      my-rect)

; Implement B
(define my-rect  (make-rect A B (make-point 8 16) D))
(print-point (pt0-rect my-rect))
(print-point (pt1-rect my-rect))
(print-point (pt2-rect my-rect))
(print-point (pt3-rect my-rect))
(perimeter my-rect)
(area      my-rect)


