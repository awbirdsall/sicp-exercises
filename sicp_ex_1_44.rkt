#lang planet neil/sicp
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
                   3))))

(define (repeated f times)
  (if (= times 1)
      f
      (compose f (repeated f (- times 1)))))
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square a)
  (* a a))

(define (impulse-maker a y)
  (lambda (x) (if (= x a)
                  y
                  0)))
(define impulse-function (impulse-maker 0 6))