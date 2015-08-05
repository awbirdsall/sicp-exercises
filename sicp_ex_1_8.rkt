#lang planet neil/sicp
(define (square x) (* x x))
(define (cbrt-iter guess x tol)
  (if (ge-cbrt? guess x tol)
      guess
      (cbrt-iter (imp-cbrt guess x) x tol)))
(define (ge-cbrt? guess x tol)
  (< (changeguess-cbrt guess x) tol))
(define (changeguess-cbrt guess x)
  (/ (abs (- (imp-cbrt guess x) guess)) guess))
(define (imp-cbrt guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))