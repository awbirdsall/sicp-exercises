#lang planet neil/sicp
; given structure to procedure
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        empty-board
        ;(list empty-board) ; couldn't get this to work
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; need to implement adjoin-position, empty-board, and safe?
(define (adjoin-position position shape prev)
  (append (list position) prev))
(define empty-board (list nil))
(define (safe? k positions)
  (let ((new-queen (car positions))
        (prev-queens (cdr positions)))
    (not (any-equal new-queen (unsafe-squares prev-queens (- k 1))))))
(define (any-equal value seq)
  (accumulate (lambda (x y) (or (= value x) y)) #f seq))
(define (unsafe-squares prev-queens k)
  (append prev-queens
          (accumulate-n - 0 (list prev-queens (enumerate-interval 1 k)))
          (accumulate-n + 0 (list prev-queens (enumerate-interval 1 k)))))
  
; use these previously defined higher-order procedures for sequences
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (accumulate (lambda (x y)
                                              (cons (car x) y))
                                            nil seqs))
            (accumulate-n op init (accumulate (lambda (x y)
                                                (cons (cdr x) y))
                                              nil seqs)))))  
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
  