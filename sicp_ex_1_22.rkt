#lang planet neil/sicp
(define (prime? n)
  (= n (smallest-divisor n)))
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

(define (search-for-primes n1 n2)
  (if (even? n1)
      (prime-check-range (+ 1 n1) n2 0)
      (prime-check-range n1 n2 0)))
(define (prime-check-range a b primes-counted)
  (cond ((= primes-counted 3) (newline))
        ((> a b) (newline))
        ((timed-prime-test a)
         (prime-check-range (+ a 2)
                            b
                            (+ primes-counted 1)))
        (else
         (prime-check-range(+ a 2)
                           b
                           primes-counted))))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)