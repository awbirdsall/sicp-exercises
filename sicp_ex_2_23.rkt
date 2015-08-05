#lang planet neil/sicp
(define (for-each proc items)
  (cond ((null? items) #t)
        (else
         (proc (car items))
         (for-each proc (cdr items)))))

(define (for-each-if proc items)
  (if (null? items)
      #t
      (and (proc (car items))
      (for-each proc (cdr items)))))