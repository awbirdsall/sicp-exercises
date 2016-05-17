#lang planet neil/sicp
(define (is-member? x l)
  (cond ((null? l) #f)
        ((eq? x (car l)) #t)
        (else (is-member? x (cdr l)))))
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (has-cycle? x)
  ; Search down each branch until reach leaf, or something
  ; previously encountered on same branch.
  ; More general search for cycle than "whether a program that
  ; tried to find the end of the list by taking successive cdrs
  ; would go into an infinite loop" in exercise statement --
  ; can have structure where cdrs terminate, but cycle in cars.
  (define (has-cycle-aux x aux)
    (cond ((not (pair? x)) #f)
          ((is-member? x aux) #t)
          (else
           (or (has-cycle-aux (car x) (cons x aux))
               (has-cycle-aux (cdr x) (cons x aux))))))
  (has-cycle-aux x '()))