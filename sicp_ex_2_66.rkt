#lang planet neil/sicp
(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) false)
        ((= given-key (key (entry tree-of-records)))
         (entry tree-of-records))
        ((< given-key (key (entry tree-of-records)))
         (lookup given-key (left-branch tree-of-records)))
        (else (lookup given-key (right-branch tree-of-records)))))

(define (key record) (car record))
(define (data record) (cdr record))
(define (make-record key data) (cons key data))

(define database
  (list (make-record 1 'Bill)
        (make-record 2 'Zelda)
        (make-record 3 'Viv)
        (make-record 4 'Virginia)))

;(define tree-db (list->tree database))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
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

(define (tree->list tree)
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