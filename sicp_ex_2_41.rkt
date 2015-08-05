#lang planet neil/sicp
(define (s-sum-triples n s)
  (map make-triple-sum
       (filter (lambda (triple) (s-sum? s triple))
               (unique-triples n))))

(define (make-triple-sum triple)
  (list (car triple) (cadr triple) (caddr triple) (+ (car triple)
                                                     (cadr triple)
                                                     (caddr triple))))

(define (s-sum? s triple)
  (= (+ (car triple) (cadr triple) (caddr triple)) s))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
; first messy solution
(define (unique-triples-mess n)
  (flatmap (lambda (ij)
             (map (lambda (k) (append ij (list k)))
                  (enumerate-interval 1 (- (cadr ij) 1))))
           (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n))))
; rewrite like bill the lizard
(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
  