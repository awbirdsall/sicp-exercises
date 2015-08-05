#lang planet neil/sicp
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  ; build a balanced tree containing the first n elements of list elts,
  ; returned as the car, and list of remaining elements, returned as cdr.
  ; First defines edge case of (cons '() elts) when constructing empty tree.
  ; Otherwise, recursively builds tree by making a tree whose left branch
  ; is the first (n-1)/2 elts of the list, and main entry and right branches
  ; are the remainder. The left and right branches are defined recursively
  ; in terms of partial-tree.
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define t1 (make-tree 7
                      (list 3
                            (list 1 '() '())
                            (list 5 '() '()))
                      (list 9
                            '()
                            (list 11 '() '()))))
(define t2 (make-tree 3
                      (list 1 '() '())
                      (list 7
                            (list 5 '() '())
                            (list 9
                                  '()
                                  (list 11 '() '())))))
(define t3 (make-tree 5
                      (make-tree 3
                                 (make-tree 1 '() '())
                                 '())
                      (make-tree 9
                                 (make-tree 7 '() '())
                                 (make-tree 11 '() '()))))