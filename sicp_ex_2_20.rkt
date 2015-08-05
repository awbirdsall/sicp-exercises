#lang planet neil/sicp
(define (same-parity a . l)
  (define (parity n) (abs (remainder n 2)))
  (define (check-parity a l)
    (cond ((null? l)
           nil)
          ((= (parity a) (parity (car l)))
           (cons (car l) (check-parity a (cdr l))))
          (else
           (check-parity a (cdr l)))))
  (cons a (check-parity a l)))