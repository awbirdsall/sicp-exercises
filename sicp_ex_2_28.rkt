#lang planet neil/sicp
(define (fringe items)
  (cond ((null? items) items)
        ((pair? (car items))
         (append (fringe (car items)) (fringe (cdr items))))
        (else
         (cons (car items) (fringe (cdr items))))))
(define (fringe-lizard items)
  (cond ((null? items) nil)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items))
                      (fringe (cdr items))))))
(define x (list (list 1 2) (list 3 4)))