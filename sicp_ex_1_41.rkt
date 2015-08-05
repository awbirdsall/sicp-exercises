#lang planet neil/sicp
(define (double f)
  (lambda (x) (f (f x))))