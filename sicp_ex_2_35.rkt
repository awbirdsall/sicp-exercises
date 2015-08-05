#lang planet neil/sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
; first idea
(define (count-leaves t)
  (accumulate (lambda (x y) (inc y)) 0 (enumerate-tree t)))
; question asks to use `map` within `sequence` argument of `accumulate`
(define (count-leaves-map t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))
(define test-tree (list 1
                        (list 2 (list 3 4) 5)
                        (list 6 7)))
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))