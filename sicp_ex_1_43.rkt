#lang planet neil/sicp
(define (repeated f times)
  (if (= times 1)
      f
      (compose f (repeated f (- times 1)))))
(define (compose f g)
  (lambda (x) (f (g x))))
(define (square a)
  (* a a))