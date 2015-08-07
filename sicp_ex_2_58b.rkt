#lang planet neil/sicp
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (sum? x)
  (and (pair? x) (eq? (operation x) '+)))
(define (addend s) (extract-single (inv-memq '+ s)))
(define (augend s) (extract-single (cdr (memq '+  s))))
(define (product? x)
  (and (pair? x) (eq? (operation x) '*)))
(define (multiplier p) (extract-single (inv-memq '* p)))
(define (multiplicand p) (extract-single (cdr (memq '* p))))
(define (extract-single items)
  (if (null? (cdr items))
      (car items)
      items))
(define (operation expr)
  (cond ((memq '+ expr) '+)
        ((memq '* expr) '*)
        (else (error "unknown expression type -- DERIV" exp))))
(define (inv-memq item x)
  (define (iter expr intermediate)
    (cond ((null? intermediate) #f)
          ((eq? item (car intermediate)) (front expr (- (length expr)
                                                        (length intermediate))))
          (else (iter expr (cdr intermediate)))))
  (iter x x))
(define (front expr n)
  (if (= n 0)
      nil
      (cons (last (reverse expr)) (front (cdr expr) (- n 1)))))
(define (last items)
  (if (null? (cdr items))
      (car items)
      (last (cdr items))))