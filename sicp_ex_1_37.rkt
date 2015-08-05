#lang planet neil/sicp
(define (cont-frac n d k)
  (define (cont-frac-recurse n d k i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-recurse n d k (+ 1 i))))))
  (cont-frac-recurse n d k 1))

(define (one-over-phi k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (one-over-phi-iter k)
  (cont-frac-iter (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k))