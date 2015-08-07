#lang planet neil/sicp
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a n n) 0))
  (try-it (+ 2 (random (- n 2)))))
(define (miller-rabin-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) #f)
        (else (miller-rabin-prime? n (- times 1)))))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square-check (expmod base (/ exp 2) m) m)
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (square-check a n)
  (if (and (not (= a 1))
           (not (= a (- n 1)))
           (= (remainder (square a) n) 1))
      0
      (square a)))
(define (square a)
  (* a a))