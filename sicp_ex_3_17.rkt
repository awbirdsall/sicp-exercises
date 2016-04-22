#lang planet neil/sicp
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (is-member? x l)
  (cond ((null? l) #f)
        ((eq? x (car l)) #t)
        (else (is-member? x (cdr l)))))
(define (count-pairs x)
  (define (count-pairs-aux x aux)
    (cond ((not (pair? x)) 0)
          ((not (is-member? x aux))
           (if (null? (car aux))
               (set-car! aux x) ; should only be first pair encountered,
               ; check above for `not pair?` should prevent issue with
               ; (car aux) ever returning to null.
               (set-cdr! (last-pair aux) (cons x '())))
           (+ (count-pairs-aux (car x) aux)
              (count-pairs-aux (cdr x) aux)
              1))
          (else
           (+ (count-pairs-aux (car x) aux)
              (count-pairs-aux (cdr x) aux)
              0))))
  (count-pairs-aux x (cons '() '())))