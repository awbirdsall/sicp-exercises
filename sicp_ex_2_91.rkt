#lang planet neil/sicp

;; SYMBOLIC ALGEBRA
; ex 2.90: implement both sparse and dense termlist representations.
; Implement as two packages, and generic termlist procedures called
; within install-polynomial-package.

; ex 2.91: implement polynomial division by using structure of div-terms
; procedure provided in exercise.

; generic termlist procedures
(define (first-term L) (apply-generic 'first-term L))
(define (rest-terms L) (apply-generic 'rest-terms L))
(define (order term) (apply-generic 'order term))
(define (coeff term) (apply-generic 'coeff term))
(define (adjoin term L) (apply-generic 'adjoin term L))
(define (empty-termlist? L) (apply-generic 'empty-termlist? L))
(define (add-terms L1 L2) (apply-generic 'add-terms L1 L2))
(define (negate-terms L) (apply-generic 'negate-terms L))
(define (mul-terms L1 L2) (apply-generic 'mul-terms L1 L2))
(define (div-terms L1 L2) (apply-generic 'div-terms L1 L2))

(define (install-polynomial-package)
  ;; interal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x) (symbol? x))
  ;; ex 2.87: =zero? for poly
  (define (=zero-poly? p)
    (if (empty-termlist? (term-list p))
        #t
        (and (=zero? (coeff (first-term (term-list p))))
             (=zero-poly? (make-poly (variable p)
                                     (rest-terms (term-list p)))))))
  ;; manipulation of poly
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  ; ex 2.88: poly subtraction implemented with generic negate op
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (negate-terms (term-list p2))))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))
  (define (negate-poly p)
    (make-poly (variable p) (negate-terms (term-list p))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (div-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))
  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero-poly?)
  'done)
        
    
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; ex 2.89
(define (install-dense-termlist)
  (define (adjoin-term term term-list)
    ; only works if term is higher order than term-list
    (cond ((=zero? (dense-coeff term)) term-list)
          ((empty-termlist? term-list) term)
          ((eq? (dense-order term) (+ (dense-order (first-term term-list)) 1))
           (cons (dense-coeff term) term-list))
          ((> (dense-order term) (+ (dense-order (first-term term-list)) 1))
           (adjoin-term term (cons 0 term-list)))
          (else (error "Invalid term to adjoin to term-list"
                       (list term term-list)))))
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))
  (define (rest-terms term-list)
    ; strip off leading 0 terms
    (cond ((null? term-list) term-list)
          ; handle edge case of (list 0)
          ((and (null? (cdr term-list))
                (eq? 0 (car term-list)))
           (the-empty-termlist))
          ((eq? 0 (cadr term-list)) (rest-terms (cdr term-list)))
          (else (cdr term-list))))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term term-order coeff)
    (define (make-term-iter result)
      (cond ((eq? (dense-order result) term-order)
             result)
            ((eq? (dense-order result) (- term-order 1))
             (make-term-iter (cons coeff result)))
            (else (make-term-iter (cons 0 result)))))
    (make-term-iter '()))
  ; Require descending order to make termlist from list of terms
  (define (make-termlist list-of-terms)
    (if (empty-termlist? list-of-terms)
        '()
        (adjoin-term (car list-of-terms)
                     (make-termlist (cdr list-of-terms)))))
  (define (dense-order term) (- (length term) 1))
  (define (dense-coeff term) (car term))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (dense-order t1) (dense-order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (dense-order t1) (dense-order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (dense-order t1)
                                (add (dense-coeff t1)
                                     (dense-coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (negate-terms L)
    (if (empty-termlist? L)
        L
        (adjoin-term
         (make-term (dense-order (first-term L))
                    (negate (dense-coeff (first-term L))))
         (negate-terms (rest-terms L)))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (dense-order t1) (dense-order t2))
                      (mul (dense-coeff t1) (dense-coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (dense-order t2) (dense-order t1))
              (list (the-empty-termlist) L1)
              (let* ((new-c (div (dense-coeff t1) (dense-coeff t2)))
                     (new-o (- (dense-order t1) (dense-order t2)))
                     (quotient-first-term (make-term new-o new-c))
                     (subtract-off (mul-terms L2
                                             (make-termlist (list quotient-first-term))))
                     (rest-of-result
                      (div-terms (add-terms L1
                                            (negate-terms subtract-off))
                                 L2)))
                (list (adjoin-term quotient-first-term (car rest-of-result))
                      (cdr rest-of-result))
                )))))
  ; interface to rest of system
  (define (tag-term L) (attach-tag 'dense-term L))
  (define (tag-termlist L) (attach-tag 'dense-termlist L))
  (put 'order '(dense-term) dense-order)
  (put 'coeff '(dense-term) dense-coeff)
  (put 'empty-termlist? '(dense-termlist) empty-termlist?)
  (put 'first-term '(dense-termlist)
       (lambda (L) (tag-term (first-term L))))
  (put 'rest-terms '(dense-termlist)
       (lambda (L) (tag-termlist (rest-terms L))))
  (put 'make 'dense-term
       (lambda (order coeff) (tag-term (make-term order coeff))))
  (put 'make 'dense-termlist
       (lambda (list-of-terms) (tag-termlist (make-termlist list-of-terms))))
  (put 'add-terms '(dense-termlist dense-termlist)
       (lambda (L1 L2) (tag-termlist (add-terms L1 L2))))
  (put 'negate-terms '(dense-termlist)
       (lambda (L) (tag-termlist (negate-terms L))))
  (put 'mul-terms '(dense-termlist dense-termlist)
       (lambda (L1 L2) (tag-termlist (mul-terms L1 L2))))
  (put 'div-terms '(dense-termlist dense-termlist)
       (lambda (L1 L2) (tag-termlist (div-terms L1 L2))))
  (put 'adjoin '(dense-term dense-termlist)
       (lambda (term termlist)
         (tag-termlist (adjoin-term term termlist))))
  'done)

(define (make-dense-term order coeff)
  ((get 'make 'dense-term) order coeff))
; assume each list-of-terms has type-tag of 'dense-term, which
; needs to be stripped off before sending to internal functions
(define (adjoin-dense-term raw-term raw-termlist)
  ((get 'adjoin '(dense-term dense-termlist))
   (contents raw-term) (contents raw-termlist)))
(define (make-dense-termlist list-of-terms)
  ((get 'make 'dense-termlist) (map contents list-of-terms)))

(define (install-sparse-termlist)
   ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (sparse-coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (make-termlist list-of-terms)
    (if (null? list-of-terms)
        list-of-terms
        (adjoin-term (car list-of-terms)
                     (make-termlist (cdr list-of-terms)))))
  (define (sparse-order term) (car term))
  (define (sparse-coeff term) (cadr term))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (sparse-order t1) (sparse-order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (sparse-order t1) (sparse-order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (sparse-order t1)
                                (add (sparse-coeff t1)
                                     (sparse-coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (negate-terms L)
    (if (empty-termlist? L)
        L
        (adjoin-term
         (make-term (sparse-order (first-term L))
                    (negate (sparse-coeff (first-term L))))
         (negate-terms (rest-terms L)))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (sparse-order t1) (sparse-order t2))
                      (mul (sparse-coeff t1) (sparse-coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (sparse-order t2) (sparse-order t1))
              (list (the-empty-termlist) L1)
              (let* ((new-c (div (sparse-coeff t1) (sparse-coeff t2)))
                     (new-o (- (sparse-order t1) (sparse-order t2)))
                     (quotient-first-term (make-term new-o new-c))
                     (subtract-off (mul-terms L2
                                             (make-termlist (list quotient-first-term))))
                     (rest-of-result
                      (div-terms (add-terms L1
                                            (negate-terms subtract-off))
                                 L2)))
                (list (adjoin-term quotient-first-term (car rest-of-result))
                      (cdr rest-of-result))
                )))))
  ;; interface to rest of system
  (define (tag-term L) (attach-tag 'sparse-term L))
  (define (tag-termlist L) (attach-tag 'sparse-termlist L))
  (put 'order '(sparse-term) sparse-order)
  (put 'coeff '(sparse-term) sparse-coeff)
  (put 'first-term '(sparse-termlist)
       (lambda (L) (tag-term (first-term L))))
  (put 'rest-terms '(sparse-termlist)
       (lambda (L) (tag-termlist (rest-terms L))))
  (put 'make 'sparse-term
       (lambda (order coeff) (tag-term (make-term order coeff))))
  (put 'make 'sparse-termlist
       (lambda (list-of-terms) (tag-termlist (make-termlist list-of-terms))))
  (put 'adjoin '(sparse-term sparse-termlist)
       (lambda (term termlist)
         (tag-termlist (adjoin-term term termlist))))
  (put 'add-terms '(sparse-termlist sparse-termlist)
       (lambda (L1 L2) (tag-termlist (add-terms L1 L2))))
  (put 'negate-terms '(sparse-termlist)
       (lambda (L) (tag-termlist (negate-terms L))))
  (put 'mul-terms '(sparse-termlist sparse-termlist)
       (lambda (L1 L2) (tag-termlist (mul-terms L1 L2))))
  (put 'div-terms '(sparse-termlist sparse-termlist)
       (lambda (L1 L2) (tag-termlist (div-terms L1 L2))))
  (put 'empty-termlist? '(sparse-termlist) empty-termlist?)
  'done)

(define (make-sparse-term order coeff)
  ((get 'make 'sparse-term) order coeff))
; For below, assume each arg has appropriate sparse type-tag, which
; needs to be stripped off before sending to internal functions
(define (adjoin-sparse-term term termlist)
  ((get 'adjoin '(sparse-term sparse-termlist))
   (contents term) (contents termlist)))
(define (make-sparse-termlist list-of-terms)
  ((get 'make 'sparse-termlist) (map contents list-of-terms)))

;; ARITHMETIC PACKAGES
; generic procedures
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; ex 2.86: use generic form of mul in square to handle multiple types
(define (square a) (mul a a))

; ex 2.86: rename generic real-part and imag-part in my code
; to my-real-part and my-imag-part to allow for use of builtins
; in parsing sqrts.
(define (my-real-part z) (apply-generic 'real-part z))
(define (my-imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (exp x y) (apply-generic 'exp x y)) ; ex 2.81

; ex 2.86: additional procedures that require generic forms
; for multi-datatype implementation of complex number components:
; (Need to include not just in 'ordinary and rational' as explicitly
; requested, but also in real, integer)
; Use two-argument version of atan.
(define (my-atan x y) (apply-generic 'my-atan x y))
(define (my-cos x) (apply-generic 'my-cos x))
(define (my-sin x) (apply-generic 'my-sin x))
(define (my-sqrt x) (apply-generic 'my-sqrt x))

; ex 2.88 generic negate for subtracting polynomials
(define (negate x) (apply-generic 'negate x))

; ex 2.83
(define tower-of-types '(integer rational real complex))
(define (raise x)
  (define (apply-raise types)
    (cond ((null? types)
           (error "type not found in tower-of-types -- RAISE" (list x types)))
          ((eq? (type-tag x) (car types))
           (if (null? (cdr types))
               x ; top of tower
               ((get-coercion (car types) (cadr types)) (contents x))))
          (else (apply-raise (cdr types)))))
  (apply-raise tower-of-types))

; ex 2.85: 'simplify' a data object by dropping down tower of types
; as far as possible.
(define (drop x)
  (if (not (get 'project (list (type-tag x))))
      x
      (if (equ? (raise (project x)) x)
          (drop (project x))
          x)))
; Install generic project as procedure within each package `put` into
; normal operations table, *not* into coercion table. This is to
; maintain compatibility with the ex 2.82 portion of apply-generic,
; which tries to coerce all arguments to a single type as a second
; strategy. We want to keep the 'safe' coercions (e.g., raising up
; tower) distinct from potentially 'unsafe' projections (e.g.,
; projecting 1.5 + 2i to 1.5). As written, this does have the
; disadvantage of being implemented separately from tower-of-types.
; For alternate implementation that defines projections as
; coercions and then traverses the tower-of-types in reverse, see
; http://jots-jottings.blogspot.com/2012/03/sicp-exercise-285-simplifying-types.html
(define (project x) (apply-generic 'project x))

; ex 2.79: generic equality predicate equ?
; Requires adding equ? to each package.
; Returns a Boolean, which is not tagged.
(define (equ? x y) (apply-generic 'equ? x y))

; ex 2.80: generic predicate =zero?
; Like 2.79, add =zero? to each package and return untagged Boolean
(define (=zero? x) (apply-generic '=zero? x))

;;; TYPE PACKAGES ;;;
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y)))) ; ex 2.81
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  ; ex 2.86
  (put 'my-atan '(scheme-number) atan)
  (put 'my-sin '(scheme-number) sin)
  (put 'my-cos '(scheme-number) cos)
  (put 'my-sqrt '(scheme-number) sqrt)
  ; ex 2.88
  (put 'negate '(scheme-number) (lambda (x) (- x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(integer integer)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  ; ex 2.88
  (put 'negate '(integer) (lambda (x) (tag (- x))))
  (put 'make 'integer
       (lambda (x) (if (integer? x)
                       (tag x)
                       (error "non-integer value -- MAKE-INTEGER" x))))
  ; ex 2.86: trig functions of integers are reals.
  ;          sqrt of integers are reals or complex
  (define (my-sqrt x)
    (if (real? (sqrt x))
        (make-real (sqrt x))
        (make-complex-from-real-imag (make-real (real-part (sqrt x)))
                                     (make-real (imag-part (sqrt x))))))
  (put 'my-atan '(integer integer) (lambda (x y) (make-real (atan x y))))
  (put 'my-sin '(integer) (lambda (x) (make-real (sin x))))
  (put 'my-cos '(integer) (lambda (x) (make-real (cos x))))
  (put 'my-sqrt '(integer) my-sqrt)
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (and (integer? n) (integer? d))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))
        (error "non-integer numerator or denominator -- MAKE-RAT" (list n d))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  ; ex 2.88
  (define (negate x)
    (make-rat (- (numer x)) (denom x)))
  (define (=zero? x)
    (= (numer x) 0))
  (define (project x)
    (round (/ (numer x) (denom x))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ? x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero? x)))
  (put 'project '(rational)
       (lambda (x) (attach-tag 'integer (project x))))
  (put 'negate '(rational)
       (lambda (x) (tag (negate x))))
  
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  ; ex 2.86: raise trig functions and sqrt to real and handle there
  ; rational->real redundant from coercion package but whatever
  (define (rational->real n)
    (make-real (/ (car n) (cdr n))))
  (put 'my-atan '(rational rational) (lambda (n1 n2) (my-atan (rational->real n1)
                                                              (rational->real n2))))
  (put 'my-sin '(rational) (lambda (n) (my-sin (rational->real n))))
  (put 'my-cos '(rational) (lambda (n) (my-cos (rational->real n))))
  (put 'my-sqrt '(rational) (lambda (n) (my-sqrt (rational->real n))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (define (project x)
    ; find a rational within 1/10^6
    (make-rational (round (* x 1000000)) 1000000))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(real real)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'project '(real) project)
  ; ex 2.88
  (put 'negate '(real) (tag (lambda (x) (- x))))
  (put 'make 'real
       (lambda (x) (if (real? x)
                       (tag x)
                       (error "non-real value -- MAKE-REAL" x))))
  ; ex 2.86: trig functions of reals are reals.
  ;          sqrt of reals are reals or complex
  (define (my-sqrt x)
    (if (real? (sqrt x))
        (make-real (sqrt x))
        (make-complex-from-real-imag (make-real (real-part (sqrt x)))
                                     (make-real (imag-part (sqrt x))))))
  (put 'my-atan '(real real) (lambda (x y) (make-real (atan x y))))
  (put 'my-sin '(real) (lambda (x) (make-real (sin x))))
  (put 'my-cos '(real) (lambda (x) (make-real (cos x))))
  (put 'my-sqrt '(real) my-sqrt)
  'done)

(define (make-real n)
  ((get 'make 'real) n))



; ex 2.86: use generic procedures for any manipulation of 
; real-part or imag-part, so that any type can be used
; (real-part and imag-part work as-is).
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (my-sqrt (add (square (real-part z))
                  (square (imag-part z)))))
  (define (angle z)
    (my-atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (my-cos a)) (mul r (my-sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (my-cos (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (my-sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (my-sqrt (add (square x) (square y)))
          (my-atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  ; ex 2.86: multiple types for parts requires use of generic add, etc.
  ; for any procedure that uses real-part or imag-part.
  ; IMPORTANT NOTE: breaks package for untagged real-part and imag-part
  (define (add-complex z1 z2)
    (make-from-real-imag (add (my-real-part z1) (my-real-part z2))
                         (add (my-imag-part z1) (my-imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (my-real-part z1) (my-real-part z2))
                         (sub (my-imag-part z1) (my-imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (complex-equ? z1 z2)
    (and (equ? (my-real-part z1) (my-real-part z2))
         (equ? (my-imag-part z1) (my-imag-part z2))))
  (define (=zero-complex? z)
    (and (=zero? (my-real-part z)) (=zero? (my-imag-part z))))
  ; ex 2.86: (real-part z) could be rational OR scheme-number, and if rational,
  ; might be dropped to something lower on tower-of-types (integer).
  (define (project z)
    (cond ((eq? (type-tag (my-real-part z)) 'scheme-number)
           (make-real (my-real-part z)))
          ((eq? (highest-type (list (my-real-part z) (make-real 1)) tower-of-types) 'real)
           (make-real (contents (raise-to-type (my-real-part z) 'real))))
          (else (error "unsupported real-part z datatype -- PROJECT " z))))
  ; ex 2.88
  (define (negate-complex z)
    (make-from-real-imag (negate (my-real-part z))
                         (negate (my-imag-part z))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (complex-equ? z1 z2)))
  (put '=zero? '(complex)
       (lambda (z) (=zero-complex? z)))
  (put 'project '(complex) project)
  (put 'negate '(complex)
       (lambda (z) (tag (negate-complex z))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ; ex 2.77: define complex-number selectors for complex numbers
  (put 'real-part '(complex) my-real-part)
  (put 'imag-part '(complex) my-imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; ex 2.81: coercion
; ex 2.86: need to tag each component of output complex number!
(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag n 0))
  ; ex 2.81: coerce to own type leads to infinite lookup loop
  ; (define (scheme-number->scheme-number n) n)
  ; (define (complex->complex z) z)
  ; (put-coercion 'scheme-number 'scheme-number
  ;              scheme-number->scheme-number)
  ; (put-coercion 'complex 'complex complex->complex)
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  ; ex 2.83: coerce tower of types
  ; use `put-coercion` to lookup by 'starting-type 'ending-type
  ; using separately-defined tower-of-types and `raise` procedure
  (define (integer->rational n)
    (make-rational n 1))
  (define (rational->real n)
    (make-real (/ (car n) (cdr n))))
  (define (real->complex n)
    (make-complex-from-real-imag (attach-tag 'real n)
                                 (attach-tag 'real 0)))
  (put-coercion 'integer 'rational integer->rational)
  (put-coercion 'rational 'real rational->real)
  (put-coercion 'real 'complex real->complex)
  'done)


;; INSTALL-ALL-PACKAGES
(define (install-all-packages)
  (install-scheme-number-package)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  (install-coercion-package)
  (install-polynomial-package)
  (install-dense-termlist)
  (install-sparse-termlist)
  'done)


; general procedures for data-directed style
; ex 2.78: use number? primitive to handle ordinary numbers
; rather than scheme-number tag
(define (attach-tag type-tag contents)
  (if (equal? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; ex 2.81: add handling of coercion
; ex 2.82: generalize coercion to multiple arguments
; ex 2.84: coerce arguments to have same type by successive raising
; 'need to devise a way to test which of two types is higher'
; make 'compatible' with rest of system, not lead to problems with
; adding new levels to tower.
; ex 2.85: use `drop` to "simplify" answers returned by apply-generic
(define (apply-generic op . args)
  (define (apply-generic-inner)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        ; first, use proc for exact types of args passed in if available
        (if proc
            (apply proc (map contents args))
            ; second, see if there are explicitly defined coercion
            ; procedures to coerce all args to one of the arg types
            ; passed in (using check-coerce and coerce-all)
            (if (and (not (all-eq? type-tags)) (check-coerce args))
                (let* ((coerced-args (check-coerce args))
                       (coerced-type-tags (map type-tag coerced-args))
                       (coerced-proc (get op coerced-type-tags)))
                  (if coerced-proc
                      (apply coerced-proc (map contents coerced-args))
                      (error "No method for these types"
                             (list op type-tags))))
                ; third, see if raise can be used to coerce all args
                ; to the highest-type using tower-of-types
                (let* ((high-type (highest-type args tower-of-types))
                       (raised-args (raise-all-to-type args high-type))
                       (raised-tags (map type-tag raised-args))
                       (raised-proc (get op raised-tags)))
                  (if raised-proc
                      (apply raised-proc (map contents raised-args))
                      (error "No method for these types"
                             (list op type-tags)))))))))
  ; drop apply-generic-inner result *if* it's a pair and can find
  ; a type-tag in tower-of-types (thanks, jot!) -- because apply-generic
  ; can also return things like #t or #f.
  (let ((result (apply-generic-inner)))
    (if (and (pair? result)
             (memq (type-tag result) tower-of-types))
        (drop result)
        result)))

(define (coerce-all args type)
  (define (coerce-iter args type result)
    (if (null? args)
        result
        (let* ((first-type (type-tag (car args)))
               (first-coerce (get-coercion first-type type)))
          (cond ((eq? first-type type)
                 (coerce-iter (cdr args)
                              type
                              (append result
                                      (list (car args)))))
                (first-coerce
                 (coerce-iter (cdr args)
                              type
                              (append result
                                      (list (first-coerce (contents (car args)))))))
                (else #f)))))
  (coerce-iter args type '()))


(define (check-coerce args)
  (define (check-coerce-helper types)
    (cond ((null? types) #f)
          ((coerce-all args (car types))
           (coerce-all args (car types)))
          (else (check-coerce-helper (cdr types)))))
  (check-coerce-helper (map type-tag args)))

(define (all-eq? args)
  (cond ((null? args) #t)
        ((null? (cdr args)) #t)
        ((eq? (car args) (cadr args))
         (all-eq? (cdr args)))
        (else #f)))

; ex 2.84
; follow http://jots-jottings.blogspot.com/2012/03/sicp-exercise-284-successive-raising.html
; make procedure for determining highest type given any number of inputs
(define (highest-type args type-tower)
  (define (filter-out-type type args2)
    (cond ((null? args2) '())
          ((eq? type (type-tag (car args2)))
           (filter-out-type type (cdr args2)))
          (else (cons (car args2) (filter-out-type type (cdr args2))))))
  (define (find-highest highest remaining-list remaining-tower)
    (cond ((null? remaining-list) highest)
          ((null? remaining-tower)
           (error "not all data in provided tower of types -- HIGHEST-TYPE"
                  (list args type-tower)))
          (else (find-highest (car remaining-tower)
                              (filter-out-type (car remaining-tower)
                                               remaining-list)
                              (cdr remaining-tower)))))
  (find-highest #f args type-tower))

(define (raise-all-to-type datum-list type)
  (map (lambda (x) (raise-to-type x type)) datum-list))
    
(define (raise-to-type datum type)
  (if (eq? type (type-tag datum))
      datum
      (raise-to-type (raise datum) type)))

; put and get from https://stackoverflow.com/a/5499256
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

; put-coercion and get-coercion using special coercion table
(define global-coercion-array '())

(define (put-coercion op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-coercion-array (put-helper (list op type) global-coercion-array)))

(define (get-coercion op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-coercion-array))