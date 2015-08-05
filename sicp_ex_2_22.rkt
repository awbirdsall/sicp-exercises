#lang planet neil/sicp
(define (square-list-iter-wrong items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
(define (square a)
  (* a a))