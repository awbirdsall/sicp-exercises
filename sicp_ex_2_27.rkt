#lang planet neil/sicp
; initial solution uses 'list?' built-in
(define (deep-reverse items)
  (cond ((null? items) items)
        ((not (list? items)) items)
        (else
         (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))))
; rewrite using 'pair?'
; (pair? (car items)) checks whether (car items) is a list itself
(define (deep-reverse-two items)
  (cond ((null? items) items)
        ((pair? (car items))
         (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))
        (else
         (append (deep-reverse (cdr items)) (list (car items))))))
         
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))
(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define z (list (list 1 2)))