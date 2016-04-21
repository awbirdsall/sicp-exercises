#lang planet neil/sicp
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (my-append! x y)
  (begin
    (set-cdr! (last-pair x) y)
    x))
(define (is-member? x l)
  (cond ((null? l) #f)
        ((eq? x (car l)) #t)
        (else (is-member? x (cdr l)))))
(define (count-pairs x)
  (define (count-pairs-aux x aux)
    (cond ((not (pair? x)) 0)
          ((not (is-member? x (cdr aux)))
           (begin
             ; this is problem! since `last-pair` of `aux` points to
             ; elements of the `x` whose pairs are counted, `my-
             ; append!` modifies not just `aux` but also `x`!
             (my-append! aux x)
             (display aux)
             (+ (count-pairs-aux (car x) aux)
                (count-pairs-aux (cdr x) aux)
                1)))
          (else
           (begin
             (my-append! aux x)
             (display aux)
             (+ (count-pairs-aux (car x) aux)
                (count-pairs-aux (cdr x) aux)
                0)))))
  (count-pairs-aux x '(dummy)))