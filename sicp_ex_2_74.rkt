#lang planet neil/sicp

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

; datatype tag constructor and selectors
(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; tree selectors and constructor
(define (entry-node tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))
(define (make-stupid-tree . entries) ; make tree from arguments with minimum programming effort
  (cond ((null? entries) '())
        ((null? (cdr entries)) (make-tree (car entries) '() '()))
        (else (make-tree (car entries)
                         (make-stupid-tree (cadr entries))
                         (apply make-stupid-tree (cddr entries))))))


; each division can then provide a package with the interface for working with their filetype
(define (install-a-package)
  ; contructor and selectors for entry
  (define (get-key entry) (car entry))
  (define (address entry) (caddr entry))
  (define (salary entry) (cadddr entry))
  (define (make-entry lname fname address salary)
    (list lname fname address salary))
  ; constructor and selectors for file: unsorted list
  (define (make-file entries-list) entries-list)
  (define (file-contents file)
    (map cdr file))
  (define (first-entry file) (car file))
  (define (rest-entries file) (cdr file))
  ; lookup within file by key: recursive linear search
  (define (lookup key file)
    (cond ((null? (file-contents file)) #f)
          ((equal? key
                   (get-key (first-entry (file-contents file))))
           (first-entry (file-contents file)))
          (else (lookup key (rest-entries file)))))
  ;; interface to rest of system
  (define (tag x) (attach-tag 'a x))
  (put 'key-entry '(a) (lambda (entry) (get-key entry)))
  (put 'keys-file '(a) (lambda (file) (map get-key file)))
  (put 'address-entry '(a) address)
  (put 'salary-entry '(a) salary)
  (put 'lookup '(a) (lambda (key file) (if (lookup key file)
                                           (tag (lookup key file))
                                           #f)))
  (put 'make-file-from-entries 'a (lambda (entries)
                                    (tag (make-file entries))))
  (put 'make-entry 'a (lambda (lname fname address salary)
                        (attach-tag 'a
                                    (make-entry lname
                                                fname
                                                address
                                                salary))))
  'done)

(define (install-b-package)
  ; contructor and selectors for entry
  (define (get-key entry) (cadar entry))
  (define (address entry) (cadr entry))
  (define (salary entry) (caddr entry))
  (define (make-entry lname fname address salary)
    (cons (list fname lname) (list address salary)))
  ; constructor for file: arbitrary unsorted tree (selectors: tree selectors)
  (define (make-file entries) (make-stupid-tree entries))
  ; lookup by key: flatten tree to list of entries and linear search
  (define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
      (if (null? tree)
          result-list
          (copy-to-list (left-branch tree)
                        (cons (entry-node tree)
                              (copy-to-list (right-branch tree)
                                            result-list)))))
    (copy-to-list tree '()))
  (define (lookup key file)
    (define (lookup-list list)
      (cond ((null? list) #f)
            ((equal? key (get-key (car list))) (car list))
            (else (lookup-list (cdr list)))))
    (lookup-list (tree->list-2 file)))
  ; interface to rest of system
  (define (tag x) (attach-tag 'b x))
  (put 'key-entry '(b) (lambda (entry) (get-key entry)))
  (put 'keys-file '(b) (lambda (file) (map get-key (tree->list-2 file))))
  (put 'address-entry '(b) address)
  (put 'salary-entry '(b) salary)
  (put 'lookup '(b) (lambda (key file) (if (lookup key file)
                                           (tag (lookup key file))
                                           #f)))
  (put 'tree->list '(b) tree->list-2)
  (put 'make-file-from-entries 'b (lambda (entries) (tag (make-file entries))))
  (put 'make-entry 'b (lambda (lname fname address salary)
                        (attach-tag 'b
                                    (make-entry lname
                                                fname
                                                address
                                                salary))))            
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these filetypes -- APPLY-GENERIC"
                 (list op type-tags))))))

; Look up a procedure based on the list of type-tags of the arguments.
; Return procedure that applies looked-up procedure to one external-arg,
; followed by the contents of the given args.
(define (apply-generic-file op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (lambda (external-arg)
            (map (lambda (contents-args)
                   (proc external-arg contents-args))
                 (map contents args)))
          (error "No method for these filetypes -- APPLY-GENERIC"
                 (list op type-tags))))))

; external constructors
(define (make-entry-a lname fname address salary)
  ((get 'make-entry 'a) lname fname address salary))
(define (make-entry-b lname fname address salary)
  ((get 'make-entry 'b) lname fname address salary))
(define (make-file-from-entries-a entries-list)
  ((get 'make-file-from-entries 'a) entries-list))
(define (make-file-from-entries-b entries-list)
  ((get 'make-file-from-entries 'b) entries-list))

; convenience functions if each datum is provided as a separate argument
(define (make-entries-a . data)
  (map (lambda (xs) (apply make-entry-a xs)) data))
(define (make-file-from-data-a . data)
  (make-file-from-entries-a (apply make-entries-a data)))
(define (make-entries-b . data)
  (map (lambda (xs) (apply make-entry-b xs)) data))
(define (make-file-from-data-b . data)
  (make-file-from-entries-b (apply make-entries-b data)))

; personnel file for Associated Assets: list of entries
(define data-a1 '(fitzgerald zelda (123 main st) 32000))
(define data-a2 '(woolf virginia (42 drive of ones own) 40000))
(define (make-entry-from-list-a data)
  (apply make-entry-a data))

; sample-a same as (make-file-from-data-a data-a1 data-a2)
(define sample-a
  (attach-tag 'a (list (attach-tag 'a data-a1)
                       (attach-tag 'a data-a2))))

; personnel file for Businesstown: tree of entries
(define data-b1 '(haigh-wood vivienne (0 wasteland) 40000))
(define data-b2 '(plath sylvia (33 bell jar dr) 35000))
(define data-b3 '(mccarthy mary (56 stones of florence) 34000))
(define data-b4 '(rhys jean (10 sargasso pl) 50000))
(define data-b5 '(bowles jane (2 serious ladies dr) 70000))

(define sample-b (attach-tag 'b (make-tree (cons '(vivienne haigh-wood) '((0 wasteland) 40000))
                            (make-tree (cons '(sylvia plath) '((33 Bell Jar Dr) 35000)) '() '())
                            (make-tree (cons '(mary mccarthy) '((56 stones of florence) 34000))
                                       (make-tree (cons '(jean rhys) '((10 sargasso pl) 50000)) '() '())
                                       (make-tree (cons '(jane bowles) '((2 serious ladies dr) 70000)) '() '())))))

; exercise part (a).
; Getting a record requires the file to be tagged with its type.
(define (get-record employee file)
  (car ((apply-generic-file 'lookup file) employee))) ; car prevents returning nested list

; exercise part (b).
; Getting a salary from a record requires the record to be tagged with its type.
; Examples:
; (get-salary (get-record 'fitzgerald sample-a))
; (get-salary (get-record 'plath sample-b))
(define (get-salary record)
  (apply-generic 'salary-entry record))

; exercise part (c).
(define (find-employee-record employee files)
  (cond ((null? files) #f)
        ((get-record employee (car files)) (get-record employee (car files)))
        (else (find-employee-record employee (cdr files)))))

; exercise part (d).
; incorporating a new dataset requires creation of a package with the necessary function
; definitions that interface with the rest of the system, and tagging of each record and the
; entire file with the appropriate type-tag. If each record in the existing file can be
; mapped over, the existing file can be converted to a 'public' file for interface with
; the system via the following function:
(define (convert-for-public file tag)
    (attach-tag tag
                (map (lambda (x) (attach-tag tag x))
                     file)))