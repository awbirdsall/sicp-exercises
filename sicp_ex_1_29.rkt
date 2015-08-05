#lang planet neil/sicp
(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (simpsons-next a) (+ a h))
  (define (simpsons-sum term a next b k)
    (cond ((> a b) 0)
          ((or (= k 0) (= k n)) (+ (term a)
                                   (simpsons-sum term (next a) next b (+ k 1) )))
          ((even? k) (+ (* 2 (term a))
                        (simpsons-sum term (next a) next b (+ k 1))))
          (else (+ (* 4 (term a))
                   (simpsons-sum term (next a) next b (+ k 1))))))
  (* (/ h 3.0)
     (simpsons-sum f a simpsons-next b 0)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube a)
  (* a a a))