#lang planet neil/sicp
(define (sum-square-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-rel-prime n)
  (define (rel-prime-n? a)
    (rel-prime? n a))
  (filtered-accumulate * 1 identity 1 inc n rel-prime-n?))

(define (rel-prime? a b)
  (= (gcd a b) 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (filtered-accumulate combiner null-value term a next b filter?)
  (if (> a b)
      null-value
      (combiner (if (filter? a) (term a) null-value)
                (filtered-accumulate combiner null-value term (next a) next b filter?))))

(define (accumulate combiner null-value term a next b)
  (filtered-accumulate combiner null-value term a next b all-true?))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (all-true? a) true)

(define (prime? n)
  (if (< n 2)
      false
      (= n (smallest-divisor n))))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square a)
  (* a a))

(define (cube a)
  (* a a a))