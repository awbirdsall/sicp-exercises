#lang planet neil/sicp
(define (pasc row col)
  (cond ((= row 0) 1)
        ((= col 0) 1)
        ((= row col) 1)
        (else (+ (pasc (- row 1) col)
                 (pasc (- row 1) (- col 1)))))) 