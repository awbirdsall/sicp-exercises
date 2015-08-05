#lang planet neil/sicp
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
; now with stupidly convoluted mul-interval implementation!
(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond ((and (> x1 0) (> y1 0)) ; both all positive
           (make-interval (* x1 y1) (* x2 y2)))
          ((and (< x2 0) (< y2 0)) ; both all negative
           (make-interval (* x2 y2) (* x1 y1)))
          ((and (spans-zero? x) (> y1 0)) ; y positive, x span
           (make-interval (* x1 y2) (* x2 y2)))
          ((and (> x1 0) (spans-zero? y)) ; x positive, y span
           (make-interval (* x2 y1) (* x2 y2)))
          ((and (< x2 0) (> y1 0)) ; x neg, y pos
           (make-interval (* x1 y2) (* x2 y1)))
          ((and (> x1 0) (< y2 0)) ; x pos, y neg
           (make-interval (* x2 y1) (* x1 y2)))
          ((and (spans-zero? x) (< y2 0)) ; y negative, x span
           (make-interval (* x2 y1) (* x1 y1)))
          ((and (< x2 0) (spans-zero? y)) ; x neg, y span
           (make-interval (* x1 y2) (* x1 y1)))
          (else ; both span 0
           (make-interval (min (* x1 y2) (* x2 y1))
                          (max (* x1 y1) (* x2 y2)))))))
(define (spans-zero? y)
  (and (<= (lower-bound y) 0) (>= (upper-bound y) 0)))
(define (div-interval x y)
  (if (spans-zero? y)
      (error "Error: can't divide by interval spanning 0.")
      (mul-interval x
            (make-interval (/ 1.0 (upper-bound y))
                           (/ 1.0 (lower-bound y))))))
(define (make-interval a b) (cons a b))
(define (upper-bound interval)
  (cdr interval))
(define (lower-bound interval)
  (car interval))
(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))
; width of sum or difference of intervals is sum of widths
; not the case for division or multiplication