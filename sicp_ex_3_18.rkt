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
  (define (has-cycle-aux x aux)
    (cond ((not (pair? x)) #f)
          ((is-member? x aux) #t)
          (else
           (if (null? (car aux))
               (set-car! aux x)
               (set-cdr! (last-pair aux) (cons x '())))
           (display aux)
           ; problem: search for cycle down (car x) branch
           ; adds to aux that is then used during the search
           ; down the (cdr x) branch -- leads to false positives
           (or (has-cycle-aux (car x) aux)
               (has-cycle-aux (cdr x) aux)))))
    (has-cycle-aux x (cons '() '())))
  
  