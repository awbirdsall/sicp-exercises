#lang planet neil/sicp
(define (is-member? x l)
  (cond ((null? l) #f)
        ((eq? x (car l)) #t)
        (else (is-member? x (cdr l)))))
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (has-cycle-constant-space? x)
  ; Use more forgiving definition of cycle from SICP:
  ; "whether a program that tried to find the end of the list
  ; by taking successive cdrs would go into an infinite loop".

  #f)