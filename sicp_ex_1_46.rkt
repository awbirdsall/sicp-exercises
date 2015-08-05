#lang planet neil/sicp

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  (lambda (x) (try x)))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve (lambda (x) (close-enough? x (f x)))
                       f)
   first-guess))

(define (sqrt x)
  (define (average a b)
    (/ (+ a b) 2))
  ((iterative-improve (lambda (y) (< (abs (- (square y) x)) tolerance))
                     (lambda (y) (average y (/ x y))))
   1.0))

(define (square a)
  (* a a))