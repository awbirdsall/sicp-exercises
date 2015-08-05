#lang planet neil/sicp
(define (cont-frac n d k)
  (define (cont-frac-recurse i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-recurse n d k (+ 1 i))))))
  (cont-frac-recurse 1))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (* x x))))
             (lambda (i) (- (* 2 i) 1))
             k))