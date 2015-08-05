#lang planet neil/sicp
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        ; subsets of s are union of all subsets that don't contain a single element (`rest`)
        ; and those that are a union of the single element and `rest`
        (append rest
                (map (lambda (x)
                       (cons (car s) x))
                     rest)))))