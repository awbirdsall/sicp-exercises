#lang planet neil/sicp

(define (cube a)
  (* a a a))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial a)
  (product identity 1 inc a))

(define (pi-approx terms)
  (define (skip-count a)
    (if (even? a)
        a
        (+ a 1)))
  (define (skip-count-offset a)
    (+ 1 (skip-count a)))
  (define (num terms)
    (product skip-count 2 inc (+ terms 1)))
  (define (denom terms)
    (product skip-count-offset 1 inc terms))
  (* 4.
     (/ (num terms)
        (denom terms))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* (term a) result))))
  (iter a 1))
