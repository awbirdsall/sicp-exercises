#lang planet neil/sicp
(define (cont-frac n d k)
  (define (cont-frac-recurse n d k i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-recurse n d k (+ 1 i))))))
  (cont-frac-recurse n d k 1))

(define (approx-e k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i) (if (= (remainder i 3) 2)
                                  (* (/ 2 3) (+ i 1))
                                  1))
                  k)))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (approx-e-iter k)
  (+ 2 (cont-frac-iter (lambda (i) 1.0)
                       (lambda (i) (if (= (remainder i 3) 2)
                                       (* (/ 2 3) (+ i 1))
                                       1))
                       k)))