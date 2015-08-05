#lang planet neil/sicp
(define (nth-root-works x n)
  (nth-root x n (floor (/ (log n) (log 2)))))

(define (nth-root x n avg-damps)
  (fixed-point ((repeated average-damp avg-damps)
                (lambda (y) (/ x (expt y (- n 1)))))
                1.0))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (repeated f times)
  (if (= times 1)
      f
      (compose f (repeated f (- times 1)))))
(define (compose f g)
  (lambda (x) (f (g x))))