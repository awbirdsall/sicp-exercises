#lang planet neil/sicp
(define (reverse list1)
  (define (swap a b)
    (if (null? a)
        b
        (swap (cdr a) (cons (car a) b))))
  (swap list1 nil))
(define (reverse-two list1)
  (if (null? list1)
      list1
      (append (reverse (cdr list1)) (list (car list1)))))